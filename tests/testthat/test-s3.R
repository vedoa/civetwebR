test_that("cw_request class is correctly assigned and printed", {
  req <- list(
    id = 1,
    method = "GET",
    path = "/test",
    query = "a=1",
    headers = c("Content-Type" = "text/plain"),
    body = charToRaw("hello")
  )
  class(req) <- "cw_request"

  out <- capture.output(print(req))
  expect_match(out[1], "<cw_request> [1]", fixed = TRUE)
  expect_match(out[2], "Method: GET", fixed = TRUE)
  expect_match(out[3], "Path:   /test", fixed = TRUE)
  expect_match(out[4], "Query:  a=1", fixed = TRUE)
  expect_match(out[5], "Headers: 1 fields", fixed = TRUE)
  expect_match(out[6], "Body:    5 bytes", fixed = TRUE)
})

test_that("cw_response class is correctly printed", {
  res <- list(
    status = 200L,
    headers = list("X-Test" = "val"),
    body = "ok"
  )
  class(res) <- "cw_response"

  out <- capture.output(print(res))
  expect_match(out[1], "<cw_response>", fixed = TRUE)
  expect_match(out[2], "Status: 200", fixed = TRUE)
  expect_match(out[3], "Headers: 1 fields", fixed = TRUE)
  expect_match(out[4], "Body:    2 bytes/chars", fixed = TRUE)
})

test_that("req_header extracts headers case-insensitively", {
  req <- list(
    headers = c("Content-Type" = "application/json", "X-Custom" = "foo")
  )
  class(req) <- "cw_request"

  expect_equal(req_header(req, "Content-Type"), "application/json")
  expect_equal(req_header(req, "content-type"), "application/json")
  expect_equal(req_header(req, "X-CUSTOM"), "foo")
  expect_null(req_header(req, "Missing"))

  expect_error(req_header(list(), "key"), "Not a cw_request")
})

test_that("req_query extracts query parameters", {
  req <- list(query_params = list(a = "1", b = "2"))
  class(req) <- "cw_request"

  expect_equal(req_query(req, "a"), "1")
  expect_equal(req_query(req, "b"), "2")
  expect_null(req_query(req, "c"))

  expect_error(req_query(list(), "key"), "Not a cw_request")
})

test_that("req_query returns NULL if query_params is missing", {
  req <- list()
  class(req) <- "cw_request"

  expect_null(req_query(req, "any"))
})
