# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
wait_for_server <- function(port, p = NULL, timeout = 5) {
  start <- Sys.time()

  repeat {
    if (!is.null(p) && !p$is_alive()) {
      stop("Server crashed:\n", p$read_error())
    }

    ok <- tryCatch(
      {
        con <- socketConnection(
          host = "127.0.0.1",
          port = port,
          open = "r+",
          blocking = TRUE
        )
        close(con)
        TRUE
      },
      error = function(e) FALSE
    )

    if (ok) {
      return(TRUE)
    }

    if (as.numeric(Sys.time() - start, units = "secs") > timeout) {
      stop("Server did not start")
    }

    Sys.sleep(0.05)
  }
}

.http_get <- function(path, port = 8080L) {
  url <- sprintf("http://127.0.0.1:%d%s", port, path)
  paste(readLines(url, warn = FALSE), collapse = "\n")
}

# ------------------------------------------------------------------------------
# Integration tests: STATIC
# ------------------------------------------------------------------------------
test_that("integration: static serving works", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  tmp <- tempdir()

  public <- file.path(tmp, "public")
  assets <- file.path(tmp, "assets")

  dir.create(public, showWarnings = FALSE)
  dir.create(assets, showWarnings = FALSE)

  writeLines("root-file", file.path(public, "file.txt"))
  writeLines("asset-file", file.path(assets, "file.txt"))
  writeLines("INDEX", file.path(public, "index.html"))

  port <- sample(10000:20000, 1)
  base <- sprintf("http://127.0.0.1:%d", port)
  pkg_path <- normalizePath(test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(tmp, port, lib_paths, pkg_path) {
      .libPaths(lib_paths)
      if (file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path)
      } else {
        library(civetwebR)
      }

      public <- file.path(tmp, "public")
      assets <- file.path(tmp, "assets")

      serve(
        port = port,
        host = "127.0.0.1",
        quiet = TRUE,
        static = list(
          list(dir = public, prefix = "/"),
          list(dir = assets, prefix = "/assets")
        )
      )
    },
    args = list(tmp, port, .libPaths(), pkg_path)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )

  wait_for_server(port, p)

  GET <- function(path) {
    con <- url(paste0(base, path))
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE), collapse = "\n")
  }

  expect_match(GET("/file.txt"), "root-file", fixed = TRUE)
  expect_match(GET("/assets/file.txt"), "asset-file", fixed = TRUE)
  expect_match(GET("/"), "INDEX", fixed = TRUE)
})

# ------------------------------------------------------------------------------
# Integration tests: ROUTING
# ------------------------------------------------------------------------------
test_that("integration: routing works", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  port <- sample(10000:20000, 1)
  base <- sprintf("http://127.0.0.1:%d", port)
  pkg_path <- normalizePath(test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(port, lib_paths, pkg_path) {
      .libPaths(lib_paths)
      if (file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path)
      } else {
        library(civetwebR)
      }

      handle("GET", "/hello", function(req) "HELLO")

      group("/api", {
        handle("GET", "/world", function(req) "WORLD")
      })

      serve(
        port = port,
        host = "127.0.0.1",
        quiet = TRUE
      )
    },
    args = list(port, .libPaths(), pkg_path)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )

  wait_for_server(port, p)

  GET <- function(path) {
    con <- url(paste0(base, path))
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE), collapse = "\n")
  }

  expect_match(GET("/hello"), "HELLO", fixed = TRUE)
  expect_match(GET("/api/world"), "WORLD", fixed = TRUE)
})

# ------------------------------------------------------------------------------
# Integration tests: Overrides
# ------------------------------------------------------------------------------
test_that("integration: static overrides routing when overlapping", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  tmp <- tempdir()

  public <- file.path(tmp, "public")
  dir.create(public, showWarnings = FALSE)

  dir.create(file.path(public, "api"), showWarnings = FALSE)
  writeLines("STATIC", file.path(public, "api", "hello"))

  port <- sample(10000:20000, 1)
  base <- sprintf("http://127.0.0.1:%d", port)
  pkg_path <- normalizePath(test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(tmp, port, lib_paths, pkg_path) {
      .libPaths(lib_paths)
      if (file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path)
      } else {
        library(civetwebR)
      }

      handle("GET", "/api/hello", function(req) "ROUTE")

      serve(
        port = port,
        host = "127.0.0.1",
        quiet = TRUE,
        static = list(
          list(dir = file.path(tmp, "public"), prefix = "/")
        )
      )
    },
    args = list(tmp, port, .libPaths(), pkg_path)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )

  wait_for_server(port, p)

  GET <- function(path) {
    con <- url(paste0(base, path))
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE), collapse = "\n")
  }

  expect_match(GET("/api/hello"), "STATIC", fixed = TRUE)
})

# ------------------------------------------------------------------------------
# Integration tests: Custom Responses
# ------------------------------------------------------------------------------

test_that("integration: custom headers and status codes work", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  port <- sample(10000:20000, 1)
  pkg_path <- normalizePath(test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(port, lib_paths, pkg_path) {
      .libPaths(lib_paths)
      if (file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path)
      } else {
        library(civetwebR)
      }

      handle("GET", "/custom", function(req) {
        list(
          status = 201L,
          headers = list("X-Test-Header" = "civetwebR"),
          body = "status-201"
        )
      })

      serve(
        port = port,
        host = "127.0.0.1",
        quiet = TRUE
      )
    },
    args = list(port, .libPaths(), pkg_path)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )

  wait_for_server(port, p)

  # Use r+ to allow both read and write
  con <- socketConnection(
    host = "127.0.0.1",
    port = port,
    open = "r+",
    blocking = TRUE
  )
  on.exit(close(con), add = TRUE)

  writeChar(
    "GET /custom HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n",
    con,
    eos = NULL
  )
  flush(con)

  # Read raw response
  lines <- readLines(con, warn = FALSE)

  # Verify Status Code (201 Created)
  # Some CivetWeb versions might use HTTP/1.0 if not explicitly upgraded
  expect_true(any(grepl("HTTP/1\\.[01] 201", lines)))

  # Verify Custom Header propagation
  expect_true(any(grepl("X-Test-Header: civetwebR", lines)))
  # Verify Default Content-Type fallback (added in C if not provided by R)
  expect_true(any(grepl("Content-Type: text/plain", lines)))
})

test_that("integration: custom status text override works", {
  skip_on_cran()
  skip_if_not_installed("callr")

  port <- sample(10000:20000, 1)
  pkg_path <- normalizePath(test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(port, lib_paths, pkg_path) {
      .libPaths(lib_paths)
      if (file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path)
      } else {
        library(civetwebR)
      }

      handle("GET", "/teapot", function(req) {
        list(
          status = 418L,
          status_text = "I am a coffee pot",
          body = "short and stout"
        )
      })

      serve(port = port, host = "127.0.0.1", quiet = TRUE)
    },
    args = list(port, .libPaths(), pkg_path)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )
  wait_for_server(port, p)

  con <- socketConnection(
    host = "127.0.0.1",
    port = port,
    open = "r+",
    blocking = TRUE
  )
  on.exit(close(con), add = TRUE)

  writeChar(
    "GET /teapot HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n",
    con,
    eos = NULL
  )
  flush(con)

  status_line <- readLines(con, n = 1, warn = FALSE)
  expect_match(status_line, "418 I am a coffee pot", fixed = TRUE)
})

# ------------------------------------------------------------------------------
# Integration tests: Limits
# ------------------------------------------------------------------------------

test_that("integration: max body size limit and body_too_large flag", {
  skip_on_cran()
  skip_if_not_installed("callr")

  port <- sample(10000:20000, 1)
  pkg_path <- normalizePath(test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(port, lib_paths, pkg_path) {
      .libPaths(lib_paths)
      if (file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path)
      } else {
        library(civetwebR)
      }

      handle("POST", "/check-size", function(req) {
        # Return the status of the flag and the actual length received
        list(
          status = 200L,
          headers = list("X-Body-Large" = as.character(req$body_too_large)),
          body = as.character(length(req$body))
        )
      })

      serve(
        port = port,
        host = "127.0.0.1",
        quiet = TRUE,
        max_body_size = 50 # Small limit for testing
      )
    },
    args = list(port, .libPaths(), pkg_path)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )
  wait_for_server(port, p)

  # 1. Test within limit
  con1 <- socketConnection(
    host = "127.0.0.1",
    port = port,
    open = "r+",
    blocking = TRUE
  )
  payload1 <- "12345" # 5 bytes
  writeChar(
    sprintf(
      "POST /check-size HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s",
      nchar(payload1),
      payload1
    ),
    con1,
    eos = NULL
  )
  flush(con1)
  res1 <- readLines(con1, warn = FALSE)
  close(con1)

  expect_true(any(grepl("X-Body-Large: FALSE", res1)))
  expect_equal(tail(res1, 1), "5")

  # 2. Test exceeding limit
  con2 <- socketConnection(
    host = "127.0.0.1",
    port = port,
    open = "r+",
    blocking = TRUE
  )
  # 100 bytes is > 50 limit
  payload2 <- paste0(rep("a", 100), collapse = "")
  writeChar(
    sprintf(
      "POST /check-size HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s",
      nchar(payload2),
      payload2
    ),
    con2,
    eos = NULL
  )
  flush(con2)
  res2 <- readLines(con2, warn = FALSE)
  close(con2)

  # Flag should be TRUE
  expect_true(any(grepl("X-Body-Large: TRUE", res2)))
  # Length should be truncated to max_body_size (50)
  expect_equal(tail(res2, 1), "50")
})
