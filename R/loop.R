# Internal native wrappers (native symbols; required with R_forceSymbols(TRUE))

.next_request <- function(timeout_ms = 100L) {
  .Call(
    civetweb_next_request_timeout,
    as.integer(timeout_ms),
    PACKAGE = "civetwebR"
  )
}

.send_response <- function(id, res) {
  .Call(civetweb_send_response, as.integer(id), res, PACKAGE = "civetwebR")
}

#' Run the CivetWeb driver loop (single-thread safe mode)
#'
#' This loop pulls requests from C, dispatches in R, and sends responses back.
#' It runs until interrupted.
#'
#' @return Invisibly returns TRUE when the loop exits.
#' @export
run_server <- function() {
  if (!.is_running()) {
    stop("Server is not running", call. = FALSE)
  }
  if (.is_loop_running()) {
    stop("Server loop is already running", call. = FALSE)
  }

  .set_loop_running(TRUE)
  on.exit(.set_loop_running(FALSE), add = TRUE)

  repeat {
    req <- .next_request(100L) # wakes every 100ms

    if (is.null(req)) {
      next
    }

    res <- .dispatch_request(req$method, req$path)
    .send_response(req$id, res)
  }

  invisible(TRUE)
}
