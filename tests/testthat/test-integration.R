# ------------------------------------------------------------------
# Fixture: start server (no loop here)
# ------------------------------------------------------------------
local_server <- function(port = 8080L) {
  clear_handlers()

  if (.is_running()) {
    try(stop_server(), silent = TRUE)
    Sys.sleep(0.1)
  }

  start_server(port)

  on.exit({
    if (.is_running()) {
      try(stop_server(), silent = TRUE)
      Sys.sleep(0.1)
    }
  }, add = TRUE)

  invisible(port)
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
# HTTP tests (DISABLED for driver-loop model)
# ------------------------------------------------------------------
test_that("HTTP tests disabled (driver loop requires concurrency)", {
  skip("HTTP integration requires concurrent loop, not supported in base tests")
})
