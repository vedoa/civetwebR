#include <R.h>
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <R_ext/Utils.h>   /* R_CheckUserInterrupt, R_ToplevelExec */

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
  #define cw_cond_broadcast WakeAllConditionVariable
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
  #define cw_cond_broadcast   pthread_cond_broadcast
  #define cw_cond_wait(c,m)   pthread_cond_wait((c),(m))
#endif

/* ---- limits ---- */
static size_t g_max_body_size = 8u * 1024u * 1024u; /* Default 8 MiB */

typedef enum {
  CW_EVENT_HTTP = 0,
  CW_EVENT_WS_CONNECT = 1,
  CW_EVENT_WS_READY = 2,
  CW_EVENT_WS_DATA = 3,
  CW_EVENT_WS_CLOSE = 4
} cw_event_t;

/* request header pair */
typedef struct cw_hdr {
  char *name;
  char *value;
} cw_hdr_t;

typedef struct cw_request {
  cw_event_t event_type;
  int id;
  char *method;
  char *path;
  char *query;               /* query_string (no leading '?') */

  cw_hdr_t *headers;         /* copied request headers */
  int num_headers;

  unsigned char *req_body;   /* request body bytes */
  size_t req_body_len;
  int req_body_too_large;

  struct mg_connection *conn;

  /* response */
  int status;
  char *status_text;
  char *content_type;
  cw_hdr_t *res_headers;
  int num_res_headers;
  unsigned char *body;
  size_t body_len;

  int responded;

  cw_mutex_t lock;
  cw_cond_t  cv;

  struct cw_request *next;
} cw_request_t;

/* GLOBAL STATE */
static cw_request_t *q_head = NULL, *q_tail = NULL;
static cw_request_t *active_head = NULL;

static cw_mutex_t g_lock;
static cw_cond_t  q_cv;

static int next_id = 1;
static int inited = 0;

/* server running flag to unblock waits on stop */
static int g_running = 0;

static void init_once(void) {
  if (inited) return;
  cw_mutex_init(&g_lock);
  cw_cond_init(&q_cv);
  inited = 1;
}

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
  free(r->query);

  if (r->headers) {
    for (int i = 0; i < r->num_headers; i++) {
      free(r->headers[i].name);
      free(r->headers[i].value);
    }
    free(r->headers);
  }

  if (r->res_headers) {
    for (int i = 0; i < r->num_res_headers; i++) {
      free(r->res_headers[i].name);
      free(r->res_headers[i].value);
    }
    free(r->res_headers);
  }

  free(r->req_body);

  /* response */
  free(r->status_text);
  free(r->content_type);
  free(r->body);

  cw_mutex_destroy(&r->lock);
#ifndef _WIN32
  pthread_cond_destroy(&r->cv);
#endif
  free(r);
}

/* timed wait (used for polling) */
static int cond_wait_ms(cw_cond_t *cv, cw_mutex_t *mtx, int ms) {
#ifdef _WIN32
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

/* ------------------------------------------------------------
 * Interrupt-safe check:
 * returns 1 if an interrupt is pending, 0 otherwise.
 * Uses R_ToplevelExec to avoid longjmp out of our context.
 * ------------------------------------------------------------ */
static void check_interrupt_cb(void *dummy) {
  (void)dummy;
  R_CheckUserInterrupt();
}

static int interrupt_pending(void) {
  return (R_ToplevelExec(check_interrupt_cb, NULL) == FALSE);
}

/* enqueue request */
static void enqueue(cw_request_t *r) {
  cw_mutex_lock(&g_lock);

  if (q_tail) q_tail->next = r;
  else q_head = r;

  q_tail = r;

  cw_cond_signal(&q_cv);
  cw_mutex_unlock(&g_lock);
}

/* Find a persistent anchor (HTTP or WS_READY) */
static cw_request_t *find_active(int id) {
  for (cw_request_t *c = active_head; c; c = c->next) {
    if (c->id == id) return c;
  }
  return NULL;
}

/* dequeue with timeout
 * returns:
 *   1 => got request (*out set)
 *   0 => timeout or stopped (*out=NULL)
 *  -1 => user interrupt detected
 */
static int dequeue_timeout(cw_request_t **out, int timeout_ms) {
  if (timeout_ms < 0) timeout_ms = 0;

  cw_mutex_lock(&g_lock);

  int remaining = timeout_ms;

  while (q_head == NULL && g_running) {
    cw_mutex_unlock(&g_lock);

    if (interrupt_pending()) {
      *out = NULL;
      return -1;
    }

    cw_mutex_lock(&g_lock);

    if (q_head != NULL || !g_running) break;
    if (remaining <= 0) break;

    int slice = remaining > 50 ? 50 : remaining;
    (void)cond_wait_ms(&q_cv, &g_lock, slice);
    remaining -= slice;
  }

  if (!g_running || q_head == NULL) {
    cw_mutex_unlock(&g_lock);
    *out = NULL;
    return 0;
  }

  cw_request_t *r = q_head;
  q_head = r->next;
  if (!q_head) q_tail = NULL;

  /* Only keep anchors in the active list */
  if (r->event_type == CW_EVENT_HTTP || r->event_type == CW_EVENT_WS_READY) {
    r->next = active_head;
    active_head = r;
  } else {
    r->next = NULL;
  }

  cw_mutex_unlock(&g_lock);

  *out = r;
  return 1;
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

/* response parser */
static void apply_response_from_R(cw_request_t *r, SEXP res) {
  r->status = 200;

  free(r->status_text);
  r->status_text = NULL;

  free(r->content_type);
  r->content_type = dup_str("text/plain");

  free(r->body);
  r->body = NULL;
  r->body_len = 0;

  if (r->res_headers) {
    for (int i = 0; i < r->num_res_headers; i++) {
      free(r->res_headers[i].name);
      free(r->res_headers[i].value);
    }
    free(r->res_headers);
  }
  r->res_headers = NULL;
  r->num_res_headers = 0;

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
    } else if (strcmp(n, "status_text") == 0) {
      SEXP st = VECTOR_ELT(res, i);
      if (TYPEOF(st) == STRSXP && LENGTH(st) >= 1 && STRING_ELT(st, 0) != NA_STRING)
        r->status_text = dup_str(CHAR(STRING_ELT(st, 0)));
    } else if (strcmp(n, "body") == 0) {
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
    } else if (strcmp(n, "headers") == 0) {
      SEXP h = VECTOR_ELT(res, i);
      if (TYPEOF(h) != VECSXP) continue;

      SEXP hn = Rf_getAttrib(h, R_NamesSymbol);
      if (hn == R_NilValue) continue;
      if (TYPEOF(hn) != STRSXP || LENGTH(hn) != LENGTH(h)) continue;

      int nh = LENGTH(h);
      r->res_headers = (cw_hdr_t *)calloc((size_t)nh, sizeof(cw_hdr_t));
      r->num_res_headers = nh;

      for (int j = 0; j < nh; j++) {
        const char *hk = CHAR(STRING_ELT(hn, j));
        SEXP hv_sexp = VECTOR_ELT(h, j);
        const char *hv = (TYPEOF(hv_sexp) == STRSXP && LENGTH(hv_sexp) > 0) 
                         ? CHAR(STRING_ELT(hv_sexp, 0)) : "";

        r->res_headers[j].name = dup_str(hk);
        r->res_headers[j].value = dup_str(hv);

        if (strcmp(hk, "Content-Type") == 0) {
          free(r->content_type);
          r->content_type = dup_str(hv);
        }
      }
    }
  }
}

/* copy headers from mg_request_info */
static void copy_headers(cw_request_t *r, const struct mg_request_info *req) {
  r->headers = NULL;
  r->num_headers = 0;

  if (!req) return;
  if (req->num_headers <= 0) return;

  int n = req->num_headers;
  if (n > 64) n = 64; /* civetweb defines array size 64 */

  r->headers = (cw_hdr_t *)calloc((size_t)n, sizeof(cw_hdr_t));
  if (!r->headers) Rf_error("OOM");

  r->num_headers = n;
  for (int i = 0; i < n; i++) {
    const char *hn = req->http_headers[i].name;
    const char *hv = req->http_headers[i].value;
    r->headers[i].name = dup_str(hn ? hn : "");
    r->headers[i].value = dup_str(hv ? hv : "");
  }
}

/* read request body via mg_read (binary) */
static void read_body(cw_request_t *r, struct mg_connection *conn, const struct mg_request_info *req) {
  r->req_body = NULL;
  r->req_body_len = 0;
  r->req_body_too_large = 0;

  if (!conn || !req) return;

  long long cl = req->content_length; /* can be -1 */
  if (cl == 0) return;

  /* allocate up to max body, read & discard rest if larger */
  size_t maxb = g_max_body_size;

  unsigned char *buf = NULL;
  size_t cap = 0;
  size_t len = 0;

  if (cl > 0) {
    cap = (size_t)cl;
    if (cap > maxb) {
      cap = maxb;
      r->req_body_too_large = 1;
    }
    buf = (unsigned char *)malloc(cap);
    if (!buf && cap > 0) Rf_error("OOM");

    /* read exactly cl bytes (or until mg_read stops) */
    size_t remaining = (size_t)cl;
    while (remaining > 0) {
      unsigned char tmp[8192];
      size_t want = remaining > sizeof(tmp) ? sizeof(tmp) : remaining;

      int nread = mg_read(conn, tmp, want);
      if (nread <= 0) break;

      /* store up to cap, discard rest */
      size_t take = (size_t)nread;
      if (len < cap) {
        size_t room = cap - len;
        size_t put = take > room ? room : take;
        memcpy(buf + len, tmp, put);
        len += put;
      } else {
        r->req_body_too_large = 1;
      }

      remaining -= take;
    }

  } else {
    /* cl == -1 (unknown): read until peer closes or no more data */
    cap = maxb;
    buf = (unsigned char *)malloc(cap);
    if (!buf) Rf_error("OOM");

    for (;;) {
      unsigned char tmp[8192];
      int nread = mg_read(conn, tmp, sizeof(tmp));
      if (nread <= 0) break;

      size_t take = (size_t)nread;
      if (len < cap) {
        size_t room = cap - len;
        size_t put = take > room ? room : take;
        memcpy(buf + len, tmp, put);
        len += put;
        if (put < take) r->req_body_too_large = 1;
      } else {
        r->req_body_too_large = 1;
      }
    }
  }

  if (len == 0) {
    free(buf);
    r->req_body = NULL;
    r->req_body_len = 0;
  } else {
    r->req_body = buf;
    r->req_body_len = len;
  }
}

/* WebSocket Handlers (NO R API) */
static int ws_connect_handler(const struct mg_connection *conn, void *cbdata) {
  return 0; // Accept all
}

static void ws_ready_handler(struct mg_connection *conn, void *cbdata) {
  cw_request_t *r = (cw_request_t *)calloc(1, sizeof(*r));
  if (!r) return;
  cw_mutex_init(&r->lock);
  cw_cond_init(&r->cv);

  cw_mutex_lock(&g_lock);
  int id = next_id++;
  r->id = id;
  r->event_type = CW_EVENT_WS_READY;
  r->conn = conn;
  r->path = dup_str(mg_get_request_info(conn)->request_uri);

  /* Persist the ID in the connection so subsequent messages use the same one */
  mg_set_user_connection_data(conn, r);
  cw_mutex_unlock(&g_lock);

  enqueue(r);
  // We don't block here for WS_READY, just notify R
}

static int ws_data_handler(struct mg_connection *conn, int bits, char *data, size_t len, void *cbdata) {
  int opcode = bits & 0xf;
  /* Only handle Text and Binary frames; let CivetWeb handle PING/PONG/CLOSE */
  if (opcode != MG_WEBSOCKET_OPCODE_TEXT && opcode != MG_WEBSOCKET_OPCODE_BINARY) {
    return 1;
  }

  cw_request_t *r = (cw_request_t *)calloc(1, sizeof(*r));
  if (!r) return 0;
  cw_mutex_init(&r->lock);
  cw_cond_init(&r->cv);

  cw_mutex_lock(&g_lock);
  void *udata = mg_get_user_connection_data(conn);
  cw_request_t *anchor = (udata) ? (cw_request_t *)udata : NULL;
  r->id = (anchor && anchor->event_type == CW_EVENT_WS_READY) ? anchor->id : 0;
  r->event_type = CW_EVENT_WS_DATA;
  r->conn = NULL; /* This is a transient event, don't store the connection pointer */
  r->req_body_len = len;
  r->req_body = (unsigned char *)malloc(len);
  if (r->req_body) memcpy(r->req_body, data, len);
  cw_mutex_unlock(&g_lock);

  enqueue(r);
  return 1; // Keep open
}

static void ws_close_handler(const struct mg_connection *conn, void *cbdata) {
  void *udata = mg_get_user_connection_data(conn);
  cw_request_t *anchor = (udata) ? (cw_request_t *)udata : NULL;
  int id = (anchor && anchor->event_type == CW_EVENT_WS_READY) ? anchor->id : 0;

  cw_mutex_lock(&g_lock);
  /* CRITICAL: Nullify the connection pointer in the anchor immediately */
  if (anchor) anchor->conn = NULL;
  cw_mutex_unlock(&g_lock);

  cw_request_t *r = (cw_request_t *)calloc(1, sizeof(*r));
  if (!r) return;
  cw_mutex_init(&r->lock);
  cw_cond_init(&r->cv);

  cw_mutex_lock(&g_lock);
  r->id = id;
  r->event_type = CW_EVENT_WS_CLOSE;
  r->conn = NULL; /* This is a transient event, don't store the connection pointer */
  cw_mutex_unlock(&g_lock);

  enqueue(r);
  mg_set_user_connection_data((struct mg_connection *)conn, NULL);
}

/* HTTP handler (NO R API) */
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
  if (!g_running) {
    cw_mutex_unlock(&g_lock);
    mg_printf(conn, "HTTP/1.1 503 Service Unavailable\r\nContent-Length: 0\r\n\r\n");
    free_req(r);
    return 1;
  }
  r->id = next_id++;
  r->event_type = CW_EVENT_HTTP;
  cw_mutex_unlock(&g_lock);

  const char *m = (req && req->request_method) ? req->request_method : "GET";
  const char *p = (req && req->request_uri)    ? req->request_uri    : "/";
  const char *q = (req && req->query_string)   ? req->query_string   : "";

  r->method = dup_str(m);
  r->path   = dup_str(p);
  r->query  = dup_str(q);

  r->conn   = conn;

  copy_headers(r, req);
  read_body(r, conn, req);

  /* response defaults */
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

  if (r->status_text) {
    /* Manual status line construction for custom status text */
    mg_printf(conn, "HTTP/1.1 %d %s\r\n", r->status, r->status_text);
    /* Note: Since we skip mg_response_header_start, we must use mg_printf for headers too
       to keep the CivetWeb state machine consistent. */
  } else {
    mg_response_header_start(conn, r->status);
  }
  
  if (r->body_len > 0) {
    char clen_buf[64];
    snprintf(clen_buf, sizeof(clen_buf), "%lu", (unsigned long)r->body_len);
    if (r->status_text) mg_printf(conn, "Content-Length: %s\r\n", clen_buf);
    else mg_response_header_add(conn, "Content-Length", clen_buf, -1);
  }

  int ct_sent = 0;
  if (r->res_headers) {
    for (int i = 0; i < r->num_res_headers; i++) {
      if (r->status_text) mg_printf(conn, "%s: %s\r\n", r->res_headers[i].name, r->res_headers[i].value);
      else mg_response_header_add(conn, r->res_headers[i].name, r->res_headers[i].value, -1);

      if (strcmp(r->res_headers[i].name, "Content-Type") == 0) {
        ct_sent = 1;
      }
    }
  }

  if (!ct_sent) {
    const char *ct = r->content_type ? r->content_type : "text/plain";
    if (r->status_text) mg_printf(conn, "Content-Type: %s\r\n", ct);
    else mg_response_header_add(conn, "Content-Type", ct, -1);
  }
  
  if (r->status_text) mg_printf(conn, "\r\n");
  else mg_response_header_send(conn);

  if (r->body_len && r->body) {
    mg_write(conn, r->body, r->body_len);
  }

  cw_mutex_lock(&g_lock);
  remove_active(r);
  cw_mutex_unlock(&g_lock);

  free_req(r);
  return 1;
}

/* R API */

static SEXP make_interrupt_sentinel(void) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 1));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(nms, 0, Rf_mkChar("interrupted"));
  Rf_setAttrib(out, R_NamesSymbol, nms);
  SET_VECTOR_ELT(out, 0, Rf_ScalarLogical(1));
  UNPROTECT(2);
  return out;
}

SEXP civetweb_next_request_timeout(SEXP timeout_ms) {
  init_once();

  int ms = Rf_asInteger(timeout_ms);
  if (ms == NA_INTEGER || ms < 0) {
    Rf_error("timeout_ms must be >= 0");
  }

  cw_request_t *r = NULL;
  int rc = dequeue_timeout(&r, ms);

  if (rc == -1) {
    return make_interrupt_sentinel();
  }

  if (rc == 0) {
    return R_NilValue;
  }

  cw_event_t type = r->event_type;
  int id = r->id;

  /* Build req list: id, method, path, query, headers, body, body_too_large, type */
  SEXP out  = PROTECT(Rf_allocVector(VECSXP, 8));
  SEXP nms  = PROTECT(Rf_allocVector(STRSXP, 8));

  SET_STRING_ELT(nms, 0, Rf_mkChar("id"));
  SET_STRING_ELT(nms, 1, Rf_mkChar("method"));
  SET_STRING_ELT(nms, 2, Rf_mkChar("path"));
  SET_STRING_ELT(nms, 3, Rf_mkChar("query"));
  SET_STRING_ELT(nms, 4, Rf_mkChar("headers"));
  SET_STRING_ELT(nms, 5, Rf_mkChar("body"));
  SET_STRING_ELT(nms, 6, Rf_mkChar("body_too_large"));
  SET_STRING_ELT(nms, 7, Rf_mkChar("type"));
  Rf_setAttrib(out, R_NamesSymbol, nms);

  SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(r->id));
  SET_VECTOR_ELT(out, 1, Rf_mkString(r->method ? r->method : ""));
  SET_VECTOR_ELT(out, 2, Rf_mkString(r->path ? r->path : ""));
  SET_VECTOR_ELT(out, 3, Rf_mkString(r->query ? r->query : ""));

  /* headers as named character vector (names may repeat) */
  SEXP hval = PROTECT(Rf_allocVector(STRSXP, r->num_headers));
  SEXP hnm  = PROTECT(Rf_allocVector(STRSXP, r->num_headers));
  for (int i = 0; i < r->num_headers; i++) {
    SET_STRING_ELT(hnm, i, Rf_mkChar(r->headers[i].name ? r->headers[i].name : ""));
    SET_STRING_ELT(hval, i, Rf_mkChar(r->headers[i].value ? r->headers[i].value : ""));
  }
  Rf_setAttrib(hval, R_NamesSymbol, hnm);
  SET_VECTOR_ELT(out, 4, hval);

  /* body as raw() */
  if (r->req_body && r->req_body_len > 0) {
    SEXP b = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t)r->req_body_len));
    memcpy(RAW(b), r->req_body, r->req_body_len);
    SET_VECTOR_ELT(out, 5, b);
    UNPROTECT(1);
  } else {
    SET_VECTOR_ELT(out, 5, Rf_allocVector(RAWSXP, 0));
  }

  SET_VECTOR_ELT(out, 6, Rf_ScalarLogical(r->req_body_too_large ? 1 : 0));
  SET_VECTOR_ELT(out, 7, Rf_ScalarInteger((int)r->event_type));

  UNPROTECT(4);

  /* WebSocket DATA and CLOSE events are transient; free them now that data is copied to R. */
  if (type == CW_EVENT_WS_DATA) { /* Transient data frame */
    free_req(r);
  } else if (type == CW_EVENT_WS_CLOSE) { /* Connection closed event */
    /* When a connection closes, find the original READY object (anchor) and clean it up. */
    cw_mutex_lock(&g_lock);
    cw_request_t *ready_obj = find_active(id);
    if (ready_obj) {
      remove_active(ready_obj);
      free_req(ready_obj);
    }
    cw_mutex_unlock(&g_lock);
    free_req(r);
  }

  return out;
}

SEXP civetweb_ws_send(SEXP idS, SEXP dataS) {
  int id = Rf_asInteger(idS);
  cw_mutex_lock(&g_lock);
  cw_request_t *r = find_active(id);
  cw_mutex_unlock(&g_lock);

  /* Check if anchor exists AND connection is still live */
  if (!r || r->event_type != CW_EVENT_WS_READY || !r->conn) {
    return Rf_ScalarLogical(0);
  }

  if (TYPEOF(dataS) == RAWSXP) {
    return Rf_ScalarLogical(mg_websocket_write(r->conn, MG_WEBSOCKET_OPCODE_BINARY, (const char *)RAW(dataS), XLENGTH(dataS)) > 0);
  } else {
    const char *txt = CHAR(STRING_ELT(dataS, 0));
    return Rf_ScalarLogical(mg_websocket_write(r->conn, MG_WEBSOCKET_OPCODE_TEXT, txt, strlen(txt)) > 0);
  }
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

/* SERVER */

SEXP civetweb_start_server(SEXP portS, SEXP hostS, SEXP threadsS, SEXP max_bodyS, SEXP timeoutS) {
  init_once();

  if (!Rf_isInteger(portS) || LENGTH(portS) < 1) {
    Rf_error("port must be an integer");
  }
  if (!Rf_isString(hostS) || LENGTH(hostS) < 1) {
    Rf_error("host must be character(1)");
  }
  if (!Rf_isInteger(threadsS) || LENGTH(threadsS) < 1) {
    Rf_error("num_threads must be an integer");
  }
  if (!Rf_isReal(max_bodyS) || LENGTH(max_bodyS) < 1) {
    Rf_error("max_body_size must be a number");
  }
  if (!Rf_isInteger(timeoutS) || LENGTH(timeoutS) < 1) {
    Rf_error("request_timeout_ms must be an integer");
  }

  int port = INTEGER(portS)[0];
  const char *host = CHAR(STRING_ELT(hostS, 0));
  int threads = INTEGER(threadsS)[0];
  g_max_body_size = (size_t)REAL(max_bodyS)[0];
  int timeout = INTEGER(timeoutS)[0];

  char addr[128];
  snprintf(addr, sizeof(addr), "%s:%d", host, port);

  char threads_buf[16];
  snprintf(threads_buf, sizeof(threads_buf), "%d", threads);

  char timeout_buf[16];
  snprintf(timeout_buf, sizeof(timeout_buf), "%d", timeout);

  const char *opts[] = {
    "listening_ports", addr,
    "num_threads", threads_buf,
    "request_timeout_ms", timeout_buf,
    NULL
  };

  cw_mutex_lock(&g_lock);
  g_running = 1;
  cw_mutex_unlock(&g_lock);

  struct mg_context *ctx = mg_start(NULL, NULL, opts);
  if (!ctx) {
    cw_mutex_lock(&g_lock);
    g_running = 0;
    cw_mutex_unlock(&g_lock);
    Rf_error("start failed");
  }

  mg_set_request_handler(ctx, "/", handler, NULL);
  mg_set_websocket_handler(ctx, "/ws", ws_connect_handler, ws_ready_handler, ws_data_handler, ws_close_handler, NULL);

  return R_MakeExternalPtr(ctx, R_NilValue, R_NilValue);
}

SEXP civetweb_stop_server(SEXP xptr) {
  init_once();

  if (TYPEOF(xptr) != EXTPTRSXP) {
    Rf_error("Invalid server pointer");
  }

  struct mg_context *ctx = (struct mg_context *)R_ExternalPtrAddr(xptr);

  cw_mutex_lock(&g_lock);
  g_running = 0;

  /* wake next_request waiters */
  cw_cond_broadcast(&q_cv);

  /* wake any request handlers waiting for a response */
  for (cw_request_t *r = q_head; r; r = r->next) {
    cw_mutex_lock(&r->lock);
    if (!r->responded) {
      r->status = 503;
      free(r->content_type);
      r->content_type = dup_str("text/plain");
      free(r->body);
      r->body = (unsigned char *)dup_str("Service Unavailable");
      r->body_len = strlen((const char *)r->body);
      r->responded = 1;
      cw_cond_signal(&r->cv);
    }
    cw_mutex_unlock(&r->lock);
  }

  for (cw_request_t *r = active_head; r; r = r->next) {
    if (r->event_type == CW_EVENT_HTTP) cw_mutex_lock(&r->lock);
    if (!r->responded) {
      r->status = 503;
      free(r->content_type);
      r->content_type = dup_str("text/plain");
      free(r->body);
      r->body = (unsigned char *)dup_str("Service Unavailable");
      r->body_len = strlen((const char *)r->body);
      r->responded = 1;
      cw_cond_signal(&r->cv);
    }
    if (r->event_type == CW_EVENT_HTTP) cw_mutex_unlock(&r->lock);
  }

  cw_mutex_unlock(&g_lock);

  if (ctx) {
    R_SetExternalPtrAddr(xptr, NULL);
    mg_stop(ctx);
  }

  return R_NilValue;
}
