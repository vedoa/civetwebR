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
  log = NULL,
  static = NULL
) {
  args <- .validate_serve_input(port, host, quiet, log)

  port <- args$port
  host <- args$host
  quiet <- args$quiet
  log <- args$log

  static_cfg <- .validate_static(static)

  static_funs <- NULL
  if (!is.null(static_cfg)) {
    static_funs <- lapply(static_cfg, function(s) {
      static_files(
        dir = s$dir,
        prefix = s$prefix,
        index = s$index,
        cache_control = s$cache_control
      )
    })
  }

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

  repeat {
    # ---- allow external stop ----
    if (!.is_running()) {
      break
    }

    req <- .next_request(100L)

    # ---- interrupt handling ----
    if (is.list(req) && isTRUE(req$interrupted)) {
      stop_server()
      break
    }

    if (is.null(req)) {
      next
    }

    # ---- logging ----
    if (!is.null(log)) {
      try(log(req), silent = TRUE)
    }

    res <- NULL

    # ---- static ----
    if (!is.null(static_funs)) {
      for (i in seq_along(static_funs)) {
        if (startsWith(req$path, static_cfg[[i]]$prefix)) {
          res <- tryCatch(
            static_funs[[i]](req),
            error = function(e) {
              list(
                status = 500L,
                headers = list(),
                body = "Internal Server Error"
              )
            }
          )
          break
        }
      }
    }

    # ---- routing ----
    if (is.null(res)) {
      res <- .dispatch_request(req$method, req$path, req = req)
    }

    .send_response(req$id, res)
  }

  invisible(NULL)
}
