.validate_is_running <- function() {
  if (.is_running()) {
    stop("Server is already running", call. = FALSE)
  }
}

.validate_is_not_running <- function() {
  if (!.is_running()) {
    stop("Server is not running", call. = FALSE)
  }
}


.validate_port <- function(port) {
  if (!is.numeric(port) || length(port) != 1L || is.na(port)) {
    stop("port must be a single number", call. = FALSE)
  }

  port <- as.integer(port)

  if (port < 1L || port > 65535L) {
    stop("port must be between 1 and 65535", call. = FALSE)
  }

  port
}

.validate_host <- function(host) {
  if (!is.character(host) || length(host) != 1L || is.na(host)) {
    stop("host must be character(1)", call. = FALSE)
  }

  host <- trimws(host)
  if (host == "") {
    stop("host must not be empty", call. = FALSE)
  }
  if (grepl("\\s", host)) {
    stop("host must not contain whitespace", call. = FALSE)
  }

  # allow bracketed IPv6 already (e.g. "[::1]")
  if (startsWith(host, "[") && endsWith(host, "]")) {
    inner <- substr(host, 2L, nchar(host) - 1L)
    if (!.is_ipv6(inner)) {
      stop("invalid IPv6 address", call. = FALSE)
    }
    return(host)
  }

  # common hostname shortcut
  if (host == "localhost") {
    return(host)
  }

  # IPv4
  if (grepl("^(\\d{1,3}\\.){3}\\d{1,3}$", host)) {
    parts <- as.integer(strsplit(host, ".", fixed = TRUE)[[1]])
    if (length(parts) == 4L && all(parts >= 0L & parts <= 255L)) {
      return(host)
    }
    stop("invalid IPv4 address", call. = FALSE)
  }

  # IPv6 (normalize to bracket form so C can safely append :port)
  if (.is_ipv6(host)) {
    return(paste0("[", host, "]"))
  }

  # hostname (lightweight)
  if (!grepl("^[A-Za-z0-9.-]+$", host)) {
    stop("invalid host format", call. = FALSE)
  }
  if (grepl("^[-.]|[-.]$", host)) {
    stop("invalid host format", call. = FALSE)
  }

  host
}

.is_ipv6 <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  x <- trimws(x)
  if (x == "") {
    return(FALSE)
  }

  # allow zone id (e.g. "fe80::1%lo0") by validating the address part only
  addr <- sub("%.*$", "", x)

  # only hex, colon, and optional embedded IPv4
  if (!grepl("^[0-9A-Fa-f:.]+$", addr)) {
    return(FALSE)
  }
  if (grepl("::.*::", addr)) {
    return(FALSE)
  } # at most one ::

  # IPv4-embedded tail (e.g. ::ffff:192.168.0.1)
  if (grepl("\\.", addr)) {
    m <- regexec("^(.*:)(\\d{1,3}(?:\\.\\d{1,3}){3})$", addr)
    mm <- regmatches(addr, m)[[1]]
    if (length(mm) != 3L) {
      return(FALSE)
    }
    ipv4 <- mm[3]
    parts <- as.integer(strsplit(ipv4, ".", fixed = TRUE)[[1]])
    if (!(length(parts) == 4L && all(parts >= 0L & parts <= 255L))) {
      return(FALSE)
    }
    addr <- paste0(mm[2], "0:0") # treat embedded IPv4 as two hextets
  }

  # split hextets, handle ::
  if (addr == "::") {
    return(TRUE)
  }

  parts <- strsplit(addr, ":", fixed = TRUE)[[1]]

  # count empty parts to detect ::
  empty <- which(parts == "")
  if (length(empty) > 0L) {
    # must be a single run at start/end or middle representing ::
    # remove the empty strings created by split around ::
    parts <- parts[parts != ""]
    # after compression, total hextets must be <= 8
    if (length(parts) > 8L) return(FALSE)
  } else {
    if (length(parts) != 8L) return(FALSE)
  }

  # each hextet: 1-4 hex digits
  if (any(!grepl("^[0-9A-Fa-f]{1,4}$", parts))) {
    return(FALSE)
  }

  TRUE
}

.validate_quiet <- function(quiet) {
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("quiet must be logical(1)", call. = FALSE)
  }

  quiet
}

.validate_log <- function(log) {
  if (!is.null(log) && !is.function(log)) {
    stop("log must be NULL or function(req)", call. = FALSE)
  }

  log
}


.validate_timeout_ms <- function(timeout_ms) {
  if (
    !is.numeric(timeout_ms) || length(timeout_ms) != 1L || is.na(timeout_ms)
  ) {
    stop("timeout_ms must be a single number", call. = FALSE)
  }

  timeout_ms <- as.integer(timeout_ms)

  if (timeout_ms < 0L) {
    stop("timeout_ms must be non-negative", call. = FALSE)
  }

  timeout_ms
}

#' @rdname serve
#' @keywords internal
.validate_serve_input <- function(port, host, quiet, log, timeout_ms) {
  list(
    port = .validate_port(port),
    host = .validate_host(host),
    quiet = .validate_quiet(quiet),
    log = .validate_log(log),
    timeout_ms = .validate_timeout_ms(timeout_ms)
  )
}

.validate_static <- function(static) {
  if (is.null(static)) {
    return(NULL)
  }
  if (!is.list(static)) {
    stop("static must be NULL or a list", call. = FALSE)
  }

  # normalize: single config -> list(config)
  if (!all(vapply(static, is.list, logical(1)))) {
    static <- list(static)
  }

  out <- vector("list", length(static))

  for (i in seq_along(static)) {
    x <- static[[i]]

    # ---- dir ----
    if (is.null(x$dir)) {
      stop("static$dir is required", call. = FALSE)
    }
    if (!is.character(x$dir) || length(x$dir) != 1L || is.na(x$dir)) {
      stop("static$dir must be character(1)", call. = FALSE)
    }
    if (!dir.exists(x$dir)) {
      stop("static$dir must be an existing directory", call. = FALSE)
    }

    # ---- prefix ----
    prefix <- if (is.null(x$prefix)) "/" else x$prefix
    if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
      stop("static$prefix must be character(1)", call. = FALSE)
    }
    prefix <- .normalize_prefix(prefix)
    if (prefix == "") {
      prefix <- "/"
    }

    # ---- index ----
    index <- if (is.null(x$index)) "index.html" else x$index
    if (!is.character(index) || length(index) < 1L || anyNA(index)) {
      stop("static$index must be a character vector", call. = FALSE)
    }

    # ---- cache_control ----
    cache_control <- x$cache_control
    if (
      !is.null(cache_control) &&
        (!is.character(cache_control) ||
          length(cache_control) != 1L ||
          is.na(cache_control))
    ) {
      stop("static$cache_control must be NULL or character(1)", call. = FALSE)
    }

    # ---- list_dirs ----
    list_dirs <- if (is.null(x$list_dirs)) FALSE else x$list_dirs
    if (!is.logical(list_dirs) || length(list_dirs) != 1L) {
      stop("static$list_dirs must be TRUE/FALSE", call. = FALSE)
    }

    # ---- template ----
    template <- x$template
    if (
      !is.null(template) &&
        (!is.character(template) || length(template) != 1L || is.na(template))
    ) {
      stop("static$template must be NULL or character(1)", call. = FALSE)
    }
    if (!is.null(template) && !file.exists(template)) {
      stop("static$template must point to an existing file", call. = FALSE)
    }

    out[[i]] <- list(
      dir = x$dir,
      prefix = prefix,
      index = index,
      cache_control = cache_control,
      list_dirs = list_dirs,
      template = template
    )
  }

  # autosort: longest prefix first (avoids "/" shadowing "/assets")
  ord <- order(
    vapply(out, function(s) nchar(s$prefix), integer(1)),
    decreasing = TRUE
  )

  out[ord]
}
