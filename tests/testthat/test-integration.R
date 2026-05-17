# ------------------------------------------------------------------
# Helper: wait for server (FIXED)
# ------------------------------------------------------------------
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

# ------------------------------------------------------------------
# Helper: HTTP GET (text)
# ------------------------------------------------------------------
.http_get <- function(path, port = 8080L) {
  url <- sprintf("http://127.0.0.1:%d%s", port, path)
  paste(readLines(url, warn = FALSE), collapse = "\n")
}

# ------------------------------------------------------------------
# Handler tests
# ------------------------------------------------------------------
test_that("handler registers for method + path", {
  clear_handlers()
  handle("GET", "/a", function(req) "ok")
  expect_true(is.function(.get_handler("GET", "/a")))
})

test_that("duplicate handler errors", {
  clear_handlers()
  handle("GET", "/a", function(req) "ok")
  expect_error(handle("GET", "/a", function(req) "again"))
})

test_that("different methods allowed same path", {
  clear_handlers()
  handle("GET", "/a", function(req) "get")
  handle("POST", "/a", function(req) "post")

  expect_true(is.function(.get_handler("GET", "/a")))
  expect_true(is.function(.get_handler("POST", "/a")))
})

# ------------------------------------------------------------------
# Dispatch tests (pure R)
# ------------------------------------------------------------------
test_that("dispatch calls handler", {
  clear_handlers()
  handle("GET", "/a", function(req) "ok")

  res <- .dispatch_request("GET", "/a")

  expect_equal(res$status, 200L)
  expect_equal(res$body, "ok")
})

test_that("missing route returns 404", {
  clear_handlers()
  res <- .dispatch_request("GET", "/missing")
  expect_equal(res$status, 404L)
})

test_that("handler error returns 500", {
  clear_handlers()
  handle("GET", "/a", function(req) stop("boom"))

  res <- .dispatch_request("GET", "/a")
  expect_equal(res$status, 500L)
})

test_that("string response is normalized", {
  clear_handlers()
  handle("GET", "/a", function(req) "ok")

  res <- .dispatch_request("GET", "/a")

  expect_equal(res$status, 200L)
  expect_type(res$headers, "list")
  expect_equal(res$body, "ok")
})

test_that("raw response passes through", {
  clear_handlers()
  handle("GET", "/bin", function(req) {
    list(
      status = 200L,
      headers = list("Content-Type" = "application/octet-stream"),
      body = charToRaw("abc")
    )
  })

  res <- .dispatch_request("GET", "/bin")

  expect_equal(res$status, 200L)
  expect_true(is.raw(res$body))
})

test_that("group applies prefix correctly", {
  clear_handlers()

  group("/api", {
    handle("GET", "/x", function(req) "ok")
  })

  expect_true(is.function(.get_handler("GET", "/api/x")))
})

# ------------------------------------------------------------------
# Integration tests: STATIC
# ------------------------------------------------------------------
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
  pkg_path <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(pkg_path, tmp, port) {
      pkgload::load_all(pkg_path)

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
    args = list(pkg_path, tmp, port)
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

# ------------------------------------------------------------------
# Integration tests: ROUTING
# ------------------------------------------------------------------
test_that("integration: routing works", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  port <- sample(10000:20000, 1)
  base <- sprintf("http://127.0.0.1:%d", port)
  pkg_path <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(pkg_path, port) {
      pkgload::load_all(pkg_path)

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
    args = list(pkg_path, port)
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

# ------------------------------------------------------------------
# Integration tests: STATIC overrides routing
# ------------------------------------------------------------------
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
  pkg_path <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(pkg_path, tmp, port) {
      pkgload::load_all(pkg_path)

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
    args = list(pkg_path, tmp, port)
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

test_that("integration: custom headers and status codes work", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  port <- sample(10000:20000, 1)
  pkg_path <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(pkg_path, port) {
      pkgload::load_all(pkg_path)

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
    args = list(pkg_path, port)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )

  wait_for_server(port, p)

  # Manually fetch via socket to see raw HTTP headers
  # standard R url() helper hides headers and status lines
  con <- socketConnection(host = "127.0.0.1", port = port, open = "w+b")
  on.exit(close(con), add = TRUE)

  writeChar("GET /custom HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n", con, eos = NULL)
  lines <- readLines(con, warn = FALSE)

  # Verify Status Code (201 Created)
  expect_true(any(grepl("HTTP/1.1 201", lines)))
  # Verify Custom Header propagation
  expect_true(any(grepl("X-Test-Header: civetwebR", lines)))
  # Verify Default Content-Type fallback (added in C if not provided by R)
  expect_true(any(grepl("Content-Type: text/plain", lines)))
})
