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
  # ---- validate inputs ----
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    stop("dir must be character(1)", call. = FALSE)
  }

  if (!is.logical(list_dirs) || length(list_dirs) != 1L || is.na(list_dirs)) {
    stop("list_dirs must be TRUE/FALSE", call. = FALSE)
  }

  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    stop("prefix must be character(1)", call. = FALSE)
  }

  if (!is.character(index) || length(index) < 1L || anyNA(index)) {
    stop("index must be a character vector", call. = FALSE)
  }

  if (
    !is.null(cache_control) &&
      (!is.character(cache_control) ||
        length(cache_control) != 1L ||
        is.na(cache_control))
  ) {
    stop("cache_control must be NULL or character(1)", call. = FALSE)
  }

  if (
    !is.null(template) &&
      (!is.character(template) || length(template) != 1L || is.na(template))
  ) {
    stop("template must be NULL or character(1)", call. = FALSE)
  }
  if (!is.null(template) && !file.exists(template)) {
    stop("template must point to an existing file", call. = FALSE)
  }

  # ---- normalize prefix ----
  prefix <- .normalize_prefix(prefix)
  if (prefix == "") {
    prefix <- "/"
  }

  # ---- normalize root directory ----
  root <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  root_slash <- if (endsWith(root, "/")) root else paste0(root, "/")

  # ---- template + assets selection ----
  use_internal_assets <- list_dirs && is.null(template)

  tpl_path <- if (!is.null(template)) {
    template
  } else {
    system.file("civetwebR", "dir_listing.html", package = utils::packageName())
  }

  asset_prefix <- if (use_internal_assets) "/civetwebR/" else prefix

  # ---- helper: boundary-safe prefix match ----
  .prefix_match <- function(path, prefix) {
    if (prefix == "/") {
      return(TRUE)
    }
    if (!startsWith(path, prefix)) {
      return(FALSE)
    }

    pfx_len <- nchar(prefix)

    # exact match of prefix
    if (nchar(path) == pfx_len) {
      return(TRUE)
    }

    # boundary: next char must be "/"
    substr(path, pfx_len + 1L, pfx_len + 1L) == "/"
  }

  # ---- helper: safe URL decode (won't throw) ----
  .safe_url_decode <- function(x) {
    tryCatch(utils::URLdecode(x), error = function(e) x)
  }

  # ---- helper: HTML escape for directory listing ----
  .html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }

  # ---- handler closure ----
  function(req) {
    path <- req$path
    if (
      is.null(path) || !is.character(path) || length(path) != 1L || is.na(path)
    ) {
      path <- "/"
    }

    # drop query string
    path <- sub("\\?.*$", "", path)

    # normalize slashes
    path <- gsub("\\\\", "/", path)

    # ensure leading slash
    if (!startsWith(path, "/")) {
      path <- paste0("/", path)
    }

    # enforce boundary-safe prefix behavior
    if (!.prefix_match(path, prefix)) {
      return(list(status = 404L, headers = list(), body = "Not Found"))
    }

    # compute relative path inside the static root
    rel <- substr(path, nchar(prefix) + 1L, nchar(path))
    rel <- sub("^/+", "", rel)

    # decode URL-encoded characters (e.g. %20)
    rel <- .safe_url_decode(rel)

    # guard against oddities early (optional but safe)
    # normalize possible Windows separators
    rel <- gsub("\\\\", "/", rel)

    # build raw filesystem path first
    file <- file.path(root, rel)

    # normalize if possible:
    # - mustWork=TRUE gives canonical path for existing targets
    # - fallback keeps something comparable for traversal checks
    file_norm <- tryCatch(
      normalizePath(file, winslash = "/", mustWork = TRUE),
      error = function(e) normalizePath(file, winslash = "/", mustWork = FALSE)
    )

    # ✅ critical fix:
    # allow the root directory itself (file_norm == root),
    # and allow anything underneath root (startsWith(file_norm, root_slash))
    if (!(identical(file_norm, root) || startsWith(file_norm, root_slash))) {
      return(list(status = 403L, headers = list(), body = "Forbidden"))
    }

    # ---- DIRECTORY ----
    if (dir.exists(file_norm)) {
      # try index files
      for (idx in index) {
        candidate <- file.path(file_norm, idx)
        if (file.exists(candidate) && !dir.exists(candidate)) {
          return(.static_response(candidate, cache_control))
        }
      }

      # directory listing (optional)
      if (isTRUE(list_dirs)) {
        files <- list.files(file_norm, all.files = FALSE, no.. = TRUE)
        is_dir <- dir.exists(file.path(file_norm, files))
        names <- ifelse(is_dir, paste0(files, "/"), files)

        # Build links using URL-ish paths (always forward slashes)
        base <- sub("/+$", "", path) # remove trailing slashes
        base <- if (base == "") "/" else paste0(base, "/")
        hrefs <- paste0(base, names)

        links <- paste0(
          '<a href="',
          .html_escape(hrefs),
          '">',
          .html_escape(names),
          "</a>"
        )

        files_html <- paste(links, collapse = "\n<br>\n")

        # read template
        if (nzchar(tpl_path) && file.exists(tpl_path)) {
          template_html <- readChar(tpl_path, file.info(tpl_path)$size)
        } else {
          template_html <- "<h1>{{path}}</h1>\n{{files}}"
        }

        body <- template_html
        body <- sub("{{path}}", .html_escape(path), body, fixed = TRUE)
        body <- sub("{{files}}", files_html, body, fixed = TRUE)
        body <- sub(
          "{{prefix}}",
          .html_escape(asset_prefix),
          body,
          fixed = TRUE
        )

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
