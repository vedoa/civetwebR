#' Start and run the HTTP server
#'
#' @param port Integer. Port to listen on.
#' @param host Character(1). Bind address.
#' @param quiet Logical. Suppress startup message.
#' @param log Function or NULL. Optional request logger.
#' @param static NULL or list. Static serving configuration(s).
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

  # validate & normalize static configuration
  static_cfg <- .validate_static(static)

  # build static handler closures in the same order as cfg
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

    # enforce longest prefix first (so "/assets" wins over "/")
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

  # boundary-safe prefix matcher:
  # - prefix "/" matches everything
  # - otherwise it must match either exactly, or followed by "/"
  .prefix_match <- function(path, prefix) {
    if (prefix == "/") {
      return(TRUE)
    }
    if (!startsWith(path, prefix)) {
      return(FALSE)
    }

    pfx_len <- nchar(prefix)

    # exact match: "/assets" matches "/assets"
    if (nchar(path) == pfx_len) {
      return(TRUE)
    }

    # boundary: "/assets/..." matches, "/assets2" does not
    substr(path, pfx_len + 1L, pfx_len + 1L) == "/"
  }

  # normalize incoming request path:
  # - ensure character(1)
  # - strip query string
  # - normalize slashes
  # - ensure leading "/"
  .normalize_path <- function(path) {
    if (
      is.null(path) || !is.character(path) || length(path) != 1L || is.na(path)
    ) {
      path <- "/"
    }
    path <- sub("\\?.*$", "", path)
    path <- gsub("\\\\", "/", path)
    if (!startsWith(path, "/")) {
      path <- paste0("/", path)
    }
    if (path == "") {
      path <- "/"
    }
    path
  }

  repeat {
    if (!.is_running()) {
      break
    }

    req <- .next_request(100L)

    # allow graceful interruption
    if (is.list(req) && isTRUE(req$interrupted)) {
      stop_server()
      break
    }

    if (is.null(req)) {
      next
    }

    # normalize path early and consistently
    path <- .normalize_path(req$path)

    # best-effort request logging
    if (!is.null(log)) {
      try(log(req), silent = TRUE)
    }

    res <- NULL

    # ---- static has priority over routing ----
    if (!is.null(static_funs)) {
      for (i in seq_along(static_funs)) {
        if (.prefix_match(path, static_cfg[[i]]$prefix)) {
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

    # ---- routing fallback ----
    if (is.null(res)) {
      res <- .dispatch_request(req$method, path, req = req)
    }

    # final safety: never send NULL
    if (is.null(res)) {
      res <- list(
        status = 500L,
        headers = list(),
        body = "Internal Server Error"
      )
    }

    .send_response(req$id, res)
  }

  invisible(NULL)
}
