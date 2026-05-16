#include <R.h>
#include <Rinternals.h>
#include <R_ext/Error.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "civetweb.h"

#ifdef _WIN32
  #include <windows.h>
  typedef CRITICAL_SECTION   cw_mutex_t;
  typedef CONDITION_VARIABLE cw_cond_t;

  #define cw_mutex_init     InitializeCriticalSection
  #define cw_mutex_lock     EnterCriticalSection
  #define cw_mutex_unlock   LeaveCriticalSection
  #define cw_mutex_destroy  DeleteCriticalSection

  #define cw_cond_init      InitializeConditionVariable
  #define cw_cond_signal    WakeConditionVariable
  /* FIX: SleepConditionVariableCS needs (cv, cs, timeout) */
  #define cw_cond_wait(c,m) SleepConditionVariableCS((c),(m),INFINITE)
#else
  #include <pthread.h>
  #include <time.h>

  typedef pthread_mutex_t cw_mutex_t;
  typedef pthread_cond_t  cw_cond_t;

  #define cw_mutex_init(m)    pthread_mutex_init((m), NULL)
  #define cw_mutex_lock       pthread_mutex_lock
  #define cw_mutex_unlock     pthread_mutex_unlock
  #define cw_mutex_destroy    pthread_mutex_destroy

  #define cw_cond_init(c)     pthread_cond_init((c), NULL)
  #define cw_cond_signal      pthread_cond_signal
  #define cw_cond_wait(c,m)   pthread_cond_wait((c),(m))
#endif

/* ============================================================ */
/* REQUEST STRUCT                                                */
/* ============================================================ */

typedef struct cw_request {
  int id;
  char *method;
  char *path;
  struct mg_connection *conn;

  int status;
  char *content_type;
  unsigned char *body;
  size_t body_len;

  int responded;

  cw_mutex_t lock;
  cw_cond_t  cv;

  struct cw_request *next;
} cw_request_t;

/* ============================================================ */
/* GLOBAL STATE                                                  */
/* ============================================================ */

static cw_request_t *q_head = NULL, *q_tail = NULL;
static cw_request_t *active_head = NULL;

static cw_mutex_t g_lock;
static cw_cond_t  q_cv;

static int next_id = 1;
static int inited = 0;

static void init_once(void) {
  if (inited) return;
  cw_mutex_init(&g_lock);
  cw_cond_init(&q_cv);
  inited = 1;
}

/* ============================================================ */
/* UTILS                                                         */
/* ============================================================ */

static char *dup_str(const char *s) {
  if (!s) s = "";
  size_t n = strlen(s);
  char *o = (char *)malloc(n + 1);
  if (!o) Rf_error("OOM");
  memcpy(o, s, n + 1);
  return o;
}

static void free_req(cw_request_t *r) {
  if (!r) return;
  free(r->method);
  free(r->path);
  free(r->content_type);
  free(r->body);
  cw_mutex_destroy(&r->lock);
#ifndef _WIN32
  pthread_cond_destroy(&r->cv);
#endif
  free(r);
}

/* ============================================================ */
/* TIMED WAIT                                                    */
/* ============================================================ */

static int cond_wait_ms(cw_cond_t *cv, cw_mutex_t *mtx, int ms) {
#ifdef _WIN32
  /* returns nonzero on success, zero on timeout/failure */
  return SleepConditionVariableCS(cv, mtx, (DWORD)ms);
#else
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);

  long ns = ts.tv_nsec + (long)ms * 1000000L;
  ts.tv_sec  += ns / 1000000000L;
  ts.tv_nsec  = ns % 1000000000L;

  return pthread_cond_timedwait(cv, mtx, &ts) == 0;
#endif
}

/* ============================================================ */
/* QUEUE                                                         */
/* ============================================================ */

static void enqueue(cw_request_t *r) {
  cw_mutex_lock(&g_lock);

  if (q_tail) q_tail->next = r;
  else q_head = r;

  q_tail = r;

  cw_cond_signal(&q_cv);
  cw_mutex_unlock(&g_lock);
}

static int dequeue_timeout(cw_request_t **out, int timeout_ms) {
  cw_request_t *r = NULL;

  cw_mutex_lock(&g_lock);

  if (!q_head) {
    (void)cond_wait_ms(&q_cv, &g_lock, timeout_ms);
  }

  if (!q_head) {
    cw_mutex_unlock(&g_lock);
    *out = NULL;
    return 0;
  }

  r = q_head;
  q_head = r->next;
  if (!q_head) q_tail = NULL;

  r->next = active_head;
  active_head = r;

  cw_mutex_unlock(&g_lock);

  *out = r;
  return 1;
}

static cw_request_t *find_active(int id) {
  for (cw_request_t *c = active_head; c; c = c->next) {
    if (c->id == id) return c;
  }
  return NULL;
}

static void remove_active(cw_request_t *r) {
  cw_request_t **pp = &active_head;
  while (*pp) {
    if (*pp == r) {
      *pp = r->next;
      return;
    }
    pp = &(*pp)->next;
  }
}

/* ============================================================ */
/* RESPONSE PARSER                                               */
/* ============================================================ */

static void apply_response_from_R(cw_request_t *r, SEXP res) {
  /* defaults */
  r->status = 200;

  free(r->content_type);
  r->content_type = dup_str("text/plain");

  free(r->body);
  r->body = NULL;
  r->body_len = 0;

  /* allow simple character(1) shortcut */
  if (TYPEOF(res) == STRSXP && LENGTH(res) >= 1) {
    SEXP s0 = STRING_ELT(res, 0);
    const char *s = (s0 == NA_STRING) ? "" : CHAR(s0);
    r->body = (unsigned char *)dup_str(s);
    r->body_len = strlen(s);
    return;
  }

  if (TYPEOF(res) != VECSXP) return;

  SEXP names = Rf_getAttrib(res, R_NamesSymbol);
  if (TYPEOF(names) != STRSXP || LENGTH(names) != LENGTH(res)) return;

  for (int i = 0; i < LENGTH(res); i++) {
    const char *n = CHAR(STRING_ELT(names, i));

    if (strcmp(n, "status") == 0) {
      int sc = Rf_asInteger(VECTOR_ELT(res, i));
      r->status = (sc == NA_INTEGER) ? 500 : sc;
    }

    else if (strcmp(n, "body") == 0) {
      SEXP b = VECTOR_ELT(res, i);

      free(r->body);
      r->body = NULL;
      r->body_len = 0;

      if (TYPEOF(b) == STRSXP && LENGTH(b) >= 1) {
        SEXP b0 = STRING_ELT(b, 0);
        const char *s = (b0 == NA_STRING) ? "" : CHAR(b0);
        r->body = (unsigned char *)dup_str(s);
        r->body_len = strlen(s);
      } else if (TYPEOF(b) == RAWSXP) {
        r->body_len = (size_t)XLENGTH(b);
        if (r->body_len > 0) {
          r->body = (unsigned char *)malloc(r->body_len);
          if (!r->body) {
            r->status = 500;
            free(r->content_type);
            r->content_type = dup_str("text/plain");
            r->body = (unsigned char *)dup_str("OOM");
            r->body_len = strlen((const char *)r->body);
          } else {
            memcpy(r->body, RAW(b), r->body_len);
          }
        }
      }
    }

    else if (strcmp(n, "headers") == 0) {
      SEXP h = VECTOR_ELT(res, i);
      if (TYPEOF(h) != VECSXP) continue;

      SEXP hn = Rf_getAttrib(h, R_NamesSymbol);
      if (TYPEOF(hn) != STRSXP || LENGTH(hn) != LENGTH(h)) continue;

      for (int j = 0; j < LENGTH(h); j++) {
        const char *hk = CHAR(STRING_ELT(hn, j));
        if (strcmp(hk, "Content-Type") != 0) continue;

        SEXP hv = VECTOR_ELT(h, j);
        if (TYPEOF(hv) == STRSXP && LENGTH(hv) >= 1 && STRING_ELT(hv, 0) != NA_STRING) {
          const char *v = CHAR(STRING_ELT(hv, 0));
          free(r->content_type);
          r->content_type = dup_str(v);
        }
      }
    }
  }
}

/* ============================================================ */
/* HTTP HANDLER (NO R API)                                       */
/* ============================================================ */

static int handler(struct mg_connection *conn, void *cbdata) {
  (void)cbdata;
  init_once();

  const struct mg_request_info *req = mg_get_request_info(conn);

  cw_request_t *r = (cw_request_t *)calloc(1, sizeof(*r));
  if (!r) {
    mg_printf(conn, "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n");
    return 1;
  }

  cw_mutex_init(&r->lock);
  cw_cond_init(&r->cv);

  cw_mutex_lock(&g_lock);
  r->id = next_id++;
  cw_mutex_unlock(&g_lock);

  const char *m = (req && req->request_method) ? req->request_method : "GET";
  const char *p = (req && req->request_uri)    ? req->request_uri    : "/";

  r->method = dup_str(m);
  r->path   = dup_str(p);
  r->conn   = conn;

  r->status = 500;
  r->content_type = dup_str("text/plain");
  r->body = NULL;
  r->body_len = 0;
  r->responded = 0;

  enqueue(r);

  cw_mutex_lock(&r->lock);
  while (!r->responded) {
    cw_cond_wait(&r->cv, &r->lock);
  }
  cw_mutex_unlock(&r->lock);

  /* FIX: avoid %zu portability issues; cast size_t -> unsigned long */
  mg_printf(conn,
    "HTTP/1.1 %d OK\r\nContent-Length: %lu\r\nContent-Type: %s\r\n\r\n",
    r->status,
    (unsigned long)r->body_len,
    r->content_type ? r->content_type : "text/plain"
  );

  if (r->body_len && r->body) {
    mg_write(conn, r->body, r->body_len);
  }

  cw_mutex_lock(&g_lock);
  remove_active(r);
  cw_mutex_unlock(&g_lock);

  free_req(r);
  return 1;
}

/* ============================================================ */
/* R API                                                         */
/* ============================================================ */

SEXP civetweb_next_request_timeout(SEXP timeout_ms) {
  init_once();

  int ms = Rf_asInteger(timeout_ms);
  if (ms == NA_INTEGER || ms < 0) {
    Rf_error("timeout_ms must be >= 0");
  }

  cw_request_t *r = NULL;
  if (!dequeue_timeout(&r, ms)) {
    return R_NilValue;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, 3));

  SET_STRING_ELT(nms, 0, Rf_mkChar("id"));
  SET_STRING_ELT(nms, 1, Rf_mkChar("method"));
  SET_STRING_ELT(nms, 2, Rf_mkChar("path"));
  Rf_setAttrib(out, R_NamesSymbol, nms);

  SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(r->id));
  SET_VECTOR_ELT(out, 1, Rf_mkString(r->method ? r->method : ""));
  SET_VECTOR_ELT(out, 2, Rf_mkString(r->path ? r->path : ""));

  UNPROTECT(2);
  return out;
}

SEXP civetweb_send_response(SEXP idS, SEXP res) {
  init_once();

  int id = Rf_asInteger(idS);
  if (id == NA_INTEGER) Rf_error("id must be integer");

  cw_mutex_lock(&g_lock);
  cw_request_t *r = find_active(id);
  cw_mutex_unlock(&g_lock);

  if (!r) Rf_error("unknown request id");

  cw_mutex_lock(&r->lock);

  apply_response_from_R(r, res);

  r->responded = 1;
  cw_cond_signal(&r->cv);

  cw_mutex_unlock(&r->lock);

  return R_NilValue;
}

/* ============================================================ */
/* SERVER                                                        */
/* ============================================================ */

SEXP civetweb_start_server(SEXP portS, SEXP hostS) {
  init_once();

  if (!Rf_isInteger(portS) || LENGTH(portS) < 1) {
    Rf_error("port must be an integer");
  }

  if (!Rf_isString(hostS) || LENGTH(hostS) < 1) {
    Rf_error("host must be character(1)");
  }

  int port = INTEGER(portS)[0];
  const char *host = CHAR(STRING_ELT(hostS, 0));

  /* build "host:port" */
  char addr[64];
  snprintf(addr, sizeof(addr), "%s:%d", host, port);

  const char *opts[] = {
    "listening_ports", addr,
    "num_threads", "1",
    NULL
  };

  struct mg_context *ctx = mg_start(NULL, NULL, opts);
  if (!ctx) Rf_error("start failed");

  mg_set_request_handler(ctx, "/", handler, NULL);

  return R_MakeExternalPtr(ctx, R_NilValue, R_NilValue);
}

SEXP civetweb_stop_server(SEXP xptr) {
  if (TYPEOF(xptr) != EXTPTRSXP) {
    Rf_error("Invalid server pointer");
  }

  struct mg_context *ctx = (struct mg_context *)R_ExternalPtrAddr(xptr);
  if (ctx) {
    R_SetExternalPtrAddr(xptr, NULL);
    mg_stop(ctx);
  }
  return R_NilValue;
}
