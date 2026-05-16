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

test_that("parse_query handles basic cases", {
  expect_equal(.parse_query("a=1"), list(a = "1"))

  expect_equal(
    .parse_query("a=1&b=2"),
    list(a = "1", b = "2")
  )

  expect_equal(
    .parse_query("flag"),
    list(flag = "")
  )

  expect_equal(
    .parse_query("name=tom%20lee"),
    list(name = "tom lee")
  )
})

test_that("query_params available in handler", {
  clear_handlers()

  handle("GET", "/test", function(req) {
    req$query_params$name
  })

  req <- list(query = "name=tom")

  res <- .dispatch_request("GET", "/test", req)

  expect_equal(res$body, "tom")
})

test_that("headers are available in handler", {
  clear_handlers()

  handle("GET", "/h", function(req) {
    req$headers[["X-Test"]]
  })

  req <- list(headers = c("X-Test" = "ok"))
  res <- .dispatch_request("GET", "/h", req = req)

  expect_equal(res$body, "ok")
})

test_that("body is available in handler (raw)", {
  clear_handlers()

  handle("POST", "/b", function(req) {
    rawToChar(req$body)
  })

  req <- list(body = charToRaw("abc"))
  res <- .dispatch_request("POST", "/b", req = req)

  expect_equal(res$body, "abc")
})

test_that("query_params + headers + body all present", {
  clear_handlers()

  handle("POST", "/all", function(req) {
    paste(
      req$query_params$a,
      req$headers[["X-T"]],
      rawToChar(req$body),
      sep = "|"
    )
  })

  req <- list(query = "a=1", headers = c("X-T" = "t"), body = charToRaw("z"))
  res <- .dispatch_request("POST", "/all", req = req)

  expect_equal(res$body, "1|t|z")
})
