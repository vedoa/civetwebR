#' Static file handler (serves files from a directory)
#'
#' Creates a handler function that serves files under a URL prefix from a local
#' directory. Intended to be used with `serve(static = ...)` or manually as a
#' handler.
#'
#' Static behavior:
#' - Requests to the prefix root (e.g. `/`, `/static`, `/static/`) can map to one
#'   of the configured index files (default: `index.html`).
#' - Directory traversal is blocked (`..` cannot escape the root directory).
#' - Response body is returned as `raw` and a basic `Content-Type` is set by
#'   extension.
#'
#' @param dir Character(1). Root directory to serve from.
#' @param prefix Character(1). URL prefix to mount under (default `"/"`).
#' @param index Character vector. Index files to try (default `"index.html"`).
#' @param cache_control NULL or character(1). If set, adds `Cache-Control`.
#'
#' @return A function `function(req)` compatible with `handle()`.
#' @export
static_files <- function(
  dir,
  prefix = "/",
  index = c("index.html"),
  cache_control = NULL
) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    stop("dir must be character(1)", call. = FALSE)
  }
  if (!is.character(index) || length(index) < 1L || anyNA(index)) {
    stop("index must be a character vector (>= 1)", call. = FALSE)
  }
  if (
    !is.null(cache_control) &&
      (!is.character(cache_control) ||
        length(cache_control) != 1L ||
        is.na(cache_control))
  ) {
    stop("cache_control must be NULL or character(1)", call. = FALSE)
  }

  prefix <- .normalize_prefix(prefix)
  if (prefix == "") {
    prefix <- "/"
  }

  root <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(root)) {
    stop("dir does not exist", call. = FALSE)
  }
  root_slash <- if (endsWith(root, "/")) root else paste0(root, "/")

  function(req) {
    path <- req$path
    if (
      is.null(path) || !is.character(path) || length(path) != 1L || is.na(path)
    ) {
      path <- "/"
    }
    path <- sub("\\?.*$", "", path)

    if (!startsWith(path, prefix)) {
      return(list(status = 404L, headers = list(), body = "Not Found"))
    }

    rel <- substr(path, nchar(prefix) + 1L, nchar(path))
    rel <- sub("^/+", "", rel)

    # if request hits prefix root, try index list
    if (rel == "" || endsWith(path, "/")) {
      for (idx in index) {
        candidate <- normalizePath(
          file.path(root, idx),
          winslash = "/",
          mustWork = FALSE
        )
        if (
          startsWith(candidate, root_slash) &&
            file.exists(candidate) &&
            !dir.exists(candidate)
        ) {
          return(.static_response(candidate, cache_control))
        }
      }
      return(list(status = 404L, headers = list(), body = "Not Found"))
    }

    file <- normalizePath(
      file.path(root, rel),
      winslash = "/",
      mustWork = FALSE
    )

    # prevent ../ traversal
    if (!startsWith(file, root_slash)) {
      return(list(status = 403L, headers = list(), body = "Forbidden"))
    }

    if (!file.exists(file) || dir.exists(file)) {
      return(list(status = 404L, headers = list(), body = "Not Found"))
    }

    .static_response(file, cache_control)
  }
}

.static_response <- function(file, cache_control) {
  size <- file.info(file)$size
  data <- readBin(file, what = "raw", n = size)

  headers <- list("Content-Type" = .guess_mime(file))
  if (!is.null(cache_control)) {
    headers[["Cache-Control"]] <- cache_control
  }

  list(status = 200L, headers = headers, body = data)
}

.guess_mime <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    html = "text/html",
    htm = "text/html",
    css = "text/css",
    js = "application/javascript",
    json = "application/json",
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    ico = "image/x-icon",
    txt = "text/plain",
    wasm = "application/wasm",
    "application/octet-stream"
  )
}
