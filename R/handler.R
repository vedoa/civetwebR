#' Register handlers under a common path prefix
#'
#' @param prefix Character(1). Path prefix (e.g. "/api/v1").
#' @param expr Code that registers handlers.
#' @return Invisibly returns TRUE.
#' @export
group <- function(prefix, expr) {
  prefix <- .normalize_prefix(prefix)

  .state$prefix_stack <- c(.state$prefix_stack, prefix)
  on.exit(
    {
      .state$prefix_stack <- utils::head(.state$prefix_stack, -1L)
    },
    add = TRUE
  )

  force(expr)
  invisible(TRUE)
}

#' Register an HTTP handler
#'
#' Registers a function for a given HTTP method and path. The path is
#' normalized and combined with the current route prefix.
#'
#' @param method Character string. HTTP method (e.g. "GET", "POST").
#' @param path Character string. Route path starting with "/".
#' @param fun Function. Handler of form function(req).
#'
#' @return TRUE (invisibly) on success.
#' @export
handle <- function(method, path, fun) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("method must be character(1)", call. = FALSE)
  }
  if (!is.function(fun)) {
    stop("fun must be a function", call. = FALSE)
  }

  method <- toupper(method)
  path <- paste0(.current_prefix(), .normalize_path(path))

  env <- .get_handlers_env()

  entry <- if (exists(path, envir = env, inherits = FALSE)) {
    get(path, envir = env, inherits = FALSE)
  } else {
    list()
  }

  if (!is.null(entry[[method]])) {
    stop("handler exists", call. = FALSE)
  }

  entry[[method]] <- fun
  assign(path, entry, envir = env)

  invisible(TRUE)
}

#' Dispatch an HTTP request
#'
#' Internal: called by driver loop
#'
#' @param method HTTP method.
#' @param path Request path.
#' @param req Request object.
.dispatch_request <- function(method, path, req = NULL) {
  fun <- .get_handler(method, path)

  if (is.null(fun)) {
    return(list(status = 404L, headers = list(), body = "Not Found"))
  }

  if (is.null(req)) {
    req <- list(method = method, path = path)
  } else {
    # ensure minimum fields are always present
    req$method <- method
    req$path <- path
    if (is.null(req$query)) {
      req$query <- ""
    }
    if (is.null(req$query_params)) {
      req$query_params <- .parse_query(req$query)
    }
    if (is.null(req$headers)) {
      req$headers <- character()
    }
    if (is.null(req$body)) req$body <- raw()
  }

  tryCatch(
    .normalize_response(fun(req)),
    error = function(e) {
      list(status = 500L, headers = list(), body = "Internal Server Error")
    }
  )
}

.parse_query <- function(q) {
  if (is.null(q) || q == "") {
    return(list())
  }
  parts <- strsplit(q, "&", fixed = TRUE)[[1]]
  out <- vector("list", length(parts))
  nms <- character(length(parts))

  for (i in seq_along(parts)) {
    p <- parts[[i]]
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    key <- utils::URLdecode(kv[[1]])
    val <- if (length(kv) >= 2L) utils::URLdecode(kv[[2]]) else ""
    nms[[i]] <- key
    out[[i]] <- val
  }

  stats::setNames(out, nms)
}

#' Normalize an HTTP response
#' @param x Object to normalize.
.normalize_response <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    x <- list(
      status = 200L,
      headers = list("Content-Type" = "text/plain"),
      body = x
    )
  }

  if (!is.list(x)) {
    stop("handler must return character(1) or a list", call. = FALSE)
  }

  if (is.null(x$status)) {
    x$status <- 200L
  } else {
    x$status <- as.integer(x$status)
  }

  if (is.null(x$headers)) {
    x$headers <- list()
  } else if (!is.list(x$headers)) {
    stop("headers must be a list", call. = FALSE)
  }

  if (is.null(x$body)) {
    x$body <- ""
  }

  class(x) <- "cw_response"
  x
}

#' Lookup an HTTP handler
#'
#' @param method HTTP method.
#' @param path Request path.
.get_handler <- function(method, path) {
  env <- .get_handlers_env()

  if (!exists(path, envir = env, inherits = FALSE)) {
    return(NULL)
  }

  entry <- get(path, envir = env, inherits = FALSE)
  entry[[toupper(method)]]
}

#' Clear all registered handlers
#'
#' @export
clear_handlers <- function() {
  env <- .get_handlers_env()
  nms <- ls(envir = env, all.names = TRUE)
  if (length(nms)) {
    rm(list = nms, envir = env)
  }
  .state$ws_handlers <- list(open = NULL, message = NULL, close = NULL)
  invisible(TRUE)
}

#' Register WebSocket event handlers
#'
#' @param event Character string: "open", "message", or "close".
#' @param fun Function taking a `cw_request` object.
#' @export
on_ws <- function(event, fun) {
  event <- match.arg(event, c("open", "message", "close"))
  if (!is.function(fun) && !is.null(fun)) {
    stop("fun must be a function")
  }
  .state$ws_handlers[[event]] <- fun
}

.dispatch_ws_event <- function(req) {
  # type mapping from server.c: 2=READY (open), 3=DATA (message), 4=CLOSE
  handler_name <- switch(
    as.character(req$type),
    "2" = "open",
    "3" = "message",
    "4" = "close",
    NULL
  )

  if (!is.null(handler_name)) {
    fun <- .state$ws_handlers[[handler_name]]
    if (is.function(fun)) {
      try(fun(req), silent = FALSE)
    }
  }
}
