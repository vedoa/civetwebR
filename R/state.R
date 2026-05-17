# Package-private state ---------------------------------------------------

# Internal environment storing server state (not exported)
.state <- new.env(parent = emptyenv())

# Native server pointer (externalptr)
.state$server_xptr <- NULL

# Driver loop state (NEW)
.state$loop_running <- FALSE

# Stack used to build nested route prefixes
.state$prefix_stack <- character()

# Registry of HTTP handlers (path -> handler metadata)
.state$handlers_env <- new.env(parent = emptyenv())

# -------------------------------------------------------------------------
# Accessors & state helpers
# -------------------------------------------------------------------------

.get_handlers_env <- function() {
  .state$handlers_env
}

.is_running <- function() {
  !is.null(.state$server_xptr)
}

.is_loop_running <- function() {
  isTRUE(.state$loop_running)
}

# -------------------------------------------------------------------------
# Path handling
# -------------------------------------------------------------------------

.normalize_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("path must be character(1)", call. = FALSE)
  }
  if (!startsWith(path, "/")) {
    stop("path must start with '/'", call. = FALSE)
  }
  path
}

.current_prefix <- function() {
  ps <- .state$prefix_stack
  if (length(ps) == 0L) {
    ""
  } else {
    paste0(ps, collapse = "")
  }
}

.normalize_prefix <- function(p) {
  if (!is.character(p) || length(p) != 1L || is.na(p)) {
    stop("prefix must be character(1)", call. = FALSE)
  }
  if (!startsWith(p, "/")) {
    p <- paste0("/", p)
  }
  sub("/+$", "", p)
}

.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.breadcrumb <- function(path) {
  parts <- strsplit(sub("^/+", "", path), "/", fixed = TRUE)[[1]]
  if (length(parts) == 1 && parts == "") {
    return('<a href="/">/</a>')
  }

  acc <- ""
  crumbs <- '<a href="/">/</a>'
  for (i in seq_along(parts)) {
    p <- parts[i]
    acc <- paste0(acc, "/", p)
    sep <- if (i == 1) " " else " / "
    crumbs <- paste0(
      crumbs,
      sep,
      '<a href="',
      acc,
      '/">',
      .html_escape(p),
      '</a>'
    )
  }
  crumbs
}

# -------------------------------------------------------------------------
# Lifecycle helpers (NEW, used by driver loop)
# -------------------------------------------------------------------------

.set_loop_running <- function(value) {
  .state$loop_running <- isTRUE(value)
}

.reset_state <- function() {
  # used internally on stop / errors to avoid stale state
  .state$server_xptr <- NULL
  .state$loop_running <- FALSE
  .state$prefix_stack <- character()

  env <- .get_handlers_env()
  nms <- ls(envir = env, all.names = TRUE)
  if (length(nms)) {
    rm(list = nms, envir = env)
  }

  invisible(TRUE)
}


.prefix_match <- function(path, prefix) {
  if (prefix == "/") {
    return(TRUE)
  }
  if (!startsWith(path, prefix)) {
    return(FALSE)
  }

  pfx_len <- nchar(prefix)
  if (nchar(path) == pfx_len) {
    return(TRUE)
  }

  substr(path, pfx_len + 1L, pfx_len + 1L) == "/"
}
