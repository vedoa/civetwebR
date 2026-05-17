#' @export
print.cw_request <- function(x, ...) {
  cat("<cw_request> [", x$id, "]\n", sep = "")
  cat("  Method: ", x$method, "\n", sep = "")
  cat("  Path:   ", x$path, "\n", sep = "")
  if (nzchar(x$query)) {
    cat("  Query:  ", x$query, "\n", sep = "")
  }

  n_hdr <- length(x$headers)
  cat("  Headers:", if (n_hdr > 0) paste(n_hdr, "fields") else "none", "\n")

  b_len <- length(x$body)
  cat("  Body:   ", if (b_len > 0) paste(b_len, "bytes") else "empty", "\n")

  invisible(x)
}

#' @export
print.cw_response <- function(x, ...) {
  cat("<cw_response>\n")
  cat("  Status: ", x$status, "\n", sep = "")

  n_hdr <- length(x$headers)
  cat("  Headers:", if (n_hdr > 0) paste(n_hdr, "fields") else "none", "\n")

  b_len <- if (is.raw(x$body)) length(x$body) else nchar(x$body)
  cat(
    "  Body:   ",
    if (b_len > 0) paste(b_len, "bytes/chars") else "empty",
    "\n"
  )

  invisible(x)
}

#' Helper to extract a header from a request
#' @param req A cw_request object.
#' @param key Header name.
#' @export
req_header <- function(req, key) {
  if (!inherits(req, "cw_request")) {
    stop("Not a cw_request", call. = FALSE)
  }

  # Search case-insensitively
  idx <- match(tolower(key), tolower(names(req$headers)))
  if (is.na(idx)) {
    return(NULL)
  }

  req$headers[[idx]]
}

#' Helper to extract a query parameter from a request
#' @param req A cw_request object.
#' @param key Parameter name.
#' @export
req_query <- function(req, key) {
  if (!inherits(req, "cw_request")) {
    stop("Not a cw_request", call. = FALSE)
  }

  if (is.null(req$query_params)) {
    # This shouldn't happen if dispatched via .dispatch_request
    return(NULL)
  }

  req$query_params[[key]]
}
