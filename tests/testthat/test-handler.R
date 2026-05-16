teardown({
  clear_handlers()
  if (!is.null(.state$server_xptr)) {
    try(stop_server(), silent = TRUE)
  }
})


test_that("handler registers for method + path", {
  clear_handlers()

  handle("GET", "/a", function(req) "ok")

  h <- .get_handler("GET", "/a")

  expect_true(is.function(h))
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