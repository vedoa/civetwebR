# Internal native wrappers (native symbols; required with R_forceSymbols(TRUE))

.next_request <- function(timeout_ms = 100L) {
  req <- .Call(
    civetweb_next_request_timeout,
    as.integer(timeout_ms),
    PACKAGE = "civetwebR"
  )

  if (is.list(req) && !isTRUE(req$interrupted)) {
    class(req) <- "cw_request"
  }
  req
}

.send_response <- function(id, res) {
  .Call(civetweb_send_response, as.integer(id), res, PACKAGE = "civetwebR")
}

#' Send a WebSocket message
#' @param id Connection ID.
#' @param data Character string or raw vector.
#' @export
ws_send <- function(id, data) {
  .Call(civetweb_ws_send, as.integer(id), data, PACKAGE = "civetwebR")
}

#' Run the CivetWeb driver loop (single-thread safe mode)
#'
#' This loop pulls requests from C, dispatches in R, and sends responses back.
#' It runs until interrupted.
#'
#' @param timeout_ms Polling timeout in milliseconds. Default is 100ms.
#' @return Invisibly returns TRUE when the loop exits.
#' @export
run_server <- function(timeout_ms = 100L) {
  if (!.is_running()) {
    stop("Server is not running", call. = FALSE)
  }
  if (.is_loop_running()) {
    stop("Server loop is already running", call. = FALSE)
  }

  timeout_ms <- .validate_timeout_ms(timeout_ms)

  .set_loop_running(TRUE)
  on.exit(.set_loop_running(FALSE), add = TRUE)

  repeat {
    req <- .next_request(timeout_ms)

    if (is.null(req)) {
      next
    }

    if (req$type == 0L) {
      # HTTP
      res <- .dispatch_request(req$method, req$path, req = req)
      .send_response(req$id, res)
    } else {
      # WebSocket events (READY, DATA, CLOSE)
      .dispatch_ws_event(req)
    }
  }

  invisible(TRUE)
}
