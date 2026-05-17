#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP civetweb_start_server(SEXP portS, SEXP hostS, SEXP threadsS, SEXP max_bodyS, SEXP timeoutS);
SEXP civetweb_stop_server(SEXP server_xptr);

/* driver-loop API */
SEXP civetweb_next_request_timeout(SEXP timeout_ms);
SEXP civetweb_send_response(SEXP id, SEXP res);
SEXP civetweb_ws_send(SEXP idS, SEXP dataS);

static const R_CallMethodDef CallEntries[] = {
    {"civetweb_start_server",        (DL_FUNC) &civetweb_start_server,        5},
    {"civetweb_stop_server",         (DL_FUNC) &civetweb_stop_server,         1},
    {"civetweb_next_request_timeout",(DL_FUNC) &civetweb_next_request_timeout,1},
    {"civetweb_send_response",       (DL_FUNC) &civetweb_send_response,       2},
    {"civetweb_ws_send",             (DL_FUNC) &civetweb_ws_send,              2},
    {NULL, NULL, 0}
};

void R_init_civetwebR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
