#' Start and run the HTTP server
#'
#' @param port Integer. Port to listen on.
#' @param host Character(1). Bind address.
#' @param quiet Logical. Suppress startup message.
#' @param log Function or NULL. Optional request logger.
#' @param static NULL or list. Static serving configuration(s).
#' @param timeout_ms Polling timeout in milliseconds. Default is 100ms.
#' @param num_threads Integer. Number of worker threads. Default is 50.
#' @param max_body_size Number. Maximum request body size in bytes. Default is 8MiB.
#' @param request_timeout_ms Integer. Socket receive timeout. Default is 30s.
#'
#' @rdname serve
#' @export
serve <- function(
  port = 8080L,
  host = "127.0.0.1",
  quiet = FALSE,
  log = NULL,
  static = NULL,
  timeout_ms = 100L,
  num_threads = 50L,
  max_body_size = 8 * 1024 * 1024,
  request_timeout_ms = 30000L
) {
  args <- .validate_serve_input(
    port,
    host,
    quiet,
    log,
    timeout_ms,
    num_threads,
    max_body_size,
    request_timeout_ms
  )

  port <- args$port
  host <- args$host
  quiet <- args$quiet
  log <- args$log
  timeout_ms <- args$timeout_ms
  num_threads <- args$num_threads
  max_body_size <- args$max_body_size
  request_timeout_ms <- args$request_timeout_ms

  static_cfg <- .validate_static(static)

  if (!is.null(static_cfg)) {
    asset_dir <- system.file("civetwebR", package = utils::packageName())
    already_mounted <- any(vapply(
      static_cfg,
      function(s) identical(s$prefix, "/civetwebR"),
      logical(1)
    ))

    if (!already_mounted && nzchar(asset_dir) && dir.exists(asset_dir)) {
      static_cfg <- c(
        static_cfg,
        list(list(
          dir = asset_dir,
          prefix = "/civetwebR",
          index = "index.html",
          cache_control = NULL,
          list_dirs = FALSE,
          template = NULL
        ))
      )
    }
  }

  # ---- build handlers ----
  static_funs <- NULL
  if (!is.null(static_cfg)) {
    static_funs <- lapply(static_cfg, function(s) {
      static_files(
        dir = s$dir,
        prefix = s$prefix,
        index = s$index,
        cache_control = s$cache_control,
        list_dirs = s$list_dirs,
        template = s$template
      )
    })

    ord <- order(
      vapply(static_cfg, function(s) nchar(s$prefix), integer(1)),
      decreasing = TRUE
    )

    static_cfg <- static_cfg[ord]
    static_funs <- static_funs[ord]
  }

  if (.is_loop_running()) {
    stop("Server loop is already running", call. = FALSE)
  }

  start_server(port, host, num_threads, max_body_size, request_timeout_ms)

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
    if (!.is_running()) {
      break
    }

    req <- .next_request(timeout_ms)

    if (is.list(req) && isTRUE(req$interrupted)) {
      stop_server()
      break
    }

    if (is.null(req)) {
      next
    }

    if (req$type != 0L) {
      .dispatch_ws_event(req)
      next
    }

    path <- .normalize_path(req$path)

    if (!is.null(log)) {
      try(log(req), silent = TRUE)
    }

    res <- NULL

    if (!is.null(static_funs)) {
      for (i in seq_along(static_funs)) {
        if (.prefix_match(path, static_cfg[[i]]$prefix)) {
          candidate <- tryCatch(
            static_funs[[i]](req),
            error = function(e) NULL
          )

          if (is.list(candidate) && !identical(candidate$status, 404L)) {
            res <- candidate
            break
          }
        }
      }
    }

    if (is.null(res)) {
      res <- .dispatch_request(req$method, path, req = req)
    }

    if (is.null(res)) {
      res <- list(
        status = 404L,
        headers = list(),
        body = "Not Found"
      )
    }

    .send_response(req$id, res)
  }

  invisible(NULL)
}
