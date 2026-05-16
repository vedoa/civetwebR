#' Static file handler (serves files from a directory)
#'
#' @param dir Character(1). Root directory to serve from.
#' @param prefix Character(1). URL prefix.
#' @param index Character vector. Index files.
#' @param cache_control NULL or character(1).
#' @param list_dirs Logical. Enable directory listing.
#' @param template Character(1) or NULL. Optional template path override.
#'
#' @export
static_files <- function(
  dir,
  prefix = "/",
  index = c("index.html"),
  cache_control = NULL,
  list_dirs = FALSE,
  template = NULL
) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    stop("dir must be character(1)", call. = FALSE)
  }

  if (!is.logical(list_dirs) || length(list_dirs) != 1L || is.na(list_dirs)) {
    stop("list_dirs must be TRUE/FALSE", call. = FALSE)
  }

  prefix <- .normalize_prefix(prefix)
  if (prefix == "") {
    prefix <- "/"
  }

  root <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  root_slash <- if (endsWith(root, "/")) root else paste0(root, "/")

  # template resolution (unchanged)
  tpl_path <- if (!is.null(template)) {
    template
  } else {
    pkg <- utils::packageName()
    if (is.null(pkg) || !nzchar(pkg)) {
      pkg <- "civetwebR"
    }

    p <- system.file("civetwebR", "dir_listing.html", package = pkg)

    if (!nzchar(p)) {
      dev_p <- file.path(
        normalizePath(".", winslash = "/", mustWork = FALSE),
        "inst",
        "civetwebR",
        "dir_listing.html"
      )
      if (file.exists(dev_p)) dev_p else ""
    } else {
      p
    }
  }

  .html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
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

  .safe_url_decode <- function(x) {
    tryCatch(utils::URLdecode(x), error = function(e) x)
  }

  .breadcrumb <- function(path) {
    parts <- strsplit(sub("^/+", "", path), "/", fixed = TRUE)[[1]]

    # root only
    if (length(parts) == 1 && parts == "") {
      return('<a href="/">/</a>')
    }

    acc <- ""
    crumbs <- '<a href="/">/</a>'

    for (i in seq_along(parts)) {
      p <- parts[i]
      acc <- paste0(acc, "/", p)

      # no " / " directly after root
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

  function(req) {
    path <- req$path
    if (is.null(path) || !is.character(path)) {
      path <- "/"
    }

    path <- sub("\\?.*$", "", path)
    path <- gsub("\\\\", "/", path)
    if (!startsWith(path, "/")) {
      path <- paste0("/", path)
    }

    if (!.prefix_match(path, prefix)) {
      return(list(status = 404L, headers = list(), body = "Not Found"))
    }

    rel <- substr(path, nchar(prefix) + 1L, nchar(path))
    rel <- sub("^/+", "", rel)
    rel <- .safe_url_decode(rel)
    rel <- gsub("\\\\", "/", rel)

    file <- file.path(root, rel)

    file_norm <- tryCatch(
      normalizePath(file, winslash = "/", mustWork = TRUE),
      error = function(e) normalizePath(file, winslash = "/", mustWork = FALSE)
    )

    if (!(identical(file_norm, root) || startsWith(file_norm, root_slash))) {
      return(list(status = 403L, headers = list(), body = "Forbidden"))
    }

    # ---- DIRECTORY ----
    if (dir.exists(file_norm)) {
      for (idx in index) {
        candidate <- file.path(file_norm, idx)
        if (file.exists(candidate) && !dir.exists(candidate)) {
          return(.static_response(candidate, cache_control))
        }
      }

      if (isTRUE(list_dirs)) {
        files <- list.files(file_norm, no.. = TRUE)
        full <- file.path(file_norm, files)
        info <- file.info(full)

        is_dir <- info$isdir
        names <- ifelse(is_dir, paste0(files, "/"), files)

        base <- sub("/+$", "", path)
        base <- if (base == "") "/" else paste0(base, "/")
        hrefs <- paste0(base, names)

        fmt_size <- function(x) {
          if (is.na(x)) {
            return("")
          }
          if (x < 1024) {
            return(paste0(x, " B"))
          }
          if (x < 1024^2) {
            return(sprintf("%.1f KB", x / 1024))
          }
          sprintf("%.1f MB", x / 1024^2)
        }

        sizes <- ifelse(is_dir, "", vapply(info$size, fmt_size, character(1)))
        times <- ifelse(
          is.na(info$mtime),
          "",
          format(info$mtime, "%Y-%m-%d %H:%M")
        )
        size_num <- ifelse(is_dir, 0, ifelse(is.na(info$size), 0, info$size))
        mtime_num <- ifelse(is.na(info$mtime), 0, as.numeric(info$mtime))
        type <- ifelse(is_dir, "dir", "file")

        links <- paste0(
          '<div class="entry" data-type="',
          type,
          '" data-name="',
          .html_escape(names),
          '" data-size="',
          size_num,
          '" data-mtime="',
          mtime_num,
          '">',
          '<a class="name" href="',
          .html_escape(hrefs),
          '">',
          .html_escape(names),
          '</a>',
          '<span class="size">',
          .html_escape(sizes),
          '</span>',
          '<span class="time">',
          .html_escape(times),
          '</span>',
          '</div>'
        )

        files_html <- paste(links, collapse = "\n")

        template_html <- if (nzchar(tpl_path) && file.exists(tpl_path)) {
          readChar(tpl_path, file.info(tpl_path)$size)
        } else {
          "<h1>{{breadcrumb}}</h1>\n{{files}}"
        }

        body <- template_html
        body <- sub("{{files}}", files_html, body, fixed = TRUE)
        body <- sub("{{breadcrumb}}", .breadcrumb(path), body, fixed = TRUE)

        return(list(
          status = 200L,
          headers = list("Content-Type" = "text/html"),
          body = body
        ))
      }

      return(list(status = 404L, headers = list(), body = "Not Found"))
    }

    # ---- FILE ----
    if (!file.exists(file_norm)) {
      return(list(status = 404L, headers = list(), body = "Not Found"))
    }

    .static_response(file_norm, cache_control)
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

  if (ext == "") {
    return("text/plain")
  }

  switch(
    ext,

    # ---- text / markup ----
    html = "text/html",
    htm = "text/html",
    css = "text/css",
    txt = "text/plain",
    md = "text/markdown",
    csv = "text/csv",
    xml = "application/xml",
    yaml = "application/x-yaml",
    yml = "application/x-yaml",

    # ---- javascript / json ----
    js = "application/javascript",
    mjs = "application/javascript",
    json = "application/json",
    map = "application/json",

    # ---- images ----
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    webp = "image/webp",
    bmp = "image/bmp",
    tiff = "image/tiff",
    ico = "image/x-icon",

    # ---- fonts ----
    woff = "font/woff",
    woff2 = "font/woff2",
    ttf = "font/ttf",
    otf = "font/otf",

    # ---- audio ----
    mp3 = "audio/mpeg",
    wav = "audio/wav",
    ogg = "audio/ogg",
    m4a = "audio/mp4",

    # ---- video ----
    mp4 = "video/mp4",
    webm = "video/webm",
    avi = "video/x-msvideo",
    mov = "video/quicktime",
    mkv = "video/x-matroska",

    # ---- archives ----
    zip = "application/zip",
    tar = "application/x-tar",
    gz = "application/gzip",
    tgz = "application/gzip",
    rar = "application/vnd.rar",

    # ---- documents ----
    pdf = "application/pdf",
    doc = "application/msword",
    docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    xls = "application/vnd.ms-excel",
    xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    ppt = "application/vnd.ms-powerpoint",
    pptx = "application/vnd.openxmlformats-officedocument.presentationml.presentation",

    # ---- binaries / wasm ----
    wasm = "application/wasm",
    exe = "application/octet-stream",
    bin = "application/octet-stream",

    # ---- fallback ----
    "application/octet-stream"
  )
}
