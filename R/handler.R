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
      .state$prefix_stack <- head(.state$prefix_stack, -1L)
    },
    add = TRUE
  )

  force(expr)
  invisible(TRUE)
}

#' Register an HTTP handler
#'
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
.dispatch_request <- function(method, path) {
  fun <- .get_handler(method, path)

  if (is.null(fun)) {
    return(list(status = 404L, headers = list(), body = "Not Found"))
  }

  req <- list(
    method = method,
    path = path
  )

  tryCatch(
    .normalize_response(fun(req)),
    error = function(e) {
      list(status = 500L, headers = list(), body = "Internal Server Error")
    }
  )
}

#' Normalize an HTTP response
.normalize_response <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "text/plain"),
      body = x
    ))
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

  x
}

#' Lookup an HTTP handler
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
  invisible(TRUE)
}
