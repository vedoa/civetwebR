#' Start and run the HTTP server
#'
#' @param port Integer. Port to listen on.
#' @param host Character(1). Bind address.
#' @param quiet Logical. Suppress startup message.
#' @param log Function or NULL. Optional request logger.
#'
#' @rdname serve
#' @export
serve <- function(
  port = 8080L,
  host = "127.0.0.1",
  quiet = FALSE,
  log = NULL
) {
  args <- .validate_serve_input(port, host, quiet, log)

  port <- args$port
  host <- args$host
  quiet <- args$quiet
  log <- args$log

  if (.is_loop_running()) {
    stop("Server loop is already running", call. = FALSE)
  }

  start_server(port, host)

  if (!quiet) {
    cat(sprintf("Server running on http://%s:%d\n", host, port))
  }

  .set_loop_running(TRUE)

  on.exit(
    {
      if (.is_running()) {
        try(stop_server(), silent = TRUE)
      }
      .set_loop_running(FALSE)
    },
    add = TRUE
  )

  tryCatch(
    {
      repeat {
        req <- .next_request(100L)

        if (is.null(req)) {
          next
        }

        if (!is.null(log)) {
          try(log(req), silent = TRUE)
        }

        res <- .dispatch_request(req$method, req$path)
        .send_response(req$id, res)
      }
    },
    interrupt = function(e) {
      invisible(NULL)
    }
  )
}


#' @rdname serve
#' @keywords internal
.validate_serve_input <- function(port, host, quiet, log) {
  list(
    port = .validate_port(port),
    host = .validate_host(host),
    quiet = .validate_quiet(quiet),
    log = .validate_log(log)
  )
}
