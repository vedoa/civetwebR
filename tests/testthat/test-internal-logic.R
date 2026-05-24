# ------------------------------------------------------------------------------
# Internal Helper Logic
# ------------------------------------------------------------------------------

test_that("path normalization logic is robust", {
  expect_error(.normalize_path("a/b"))
  expect_equal(.normalize_path("/a/b?q=1"), "/a/b?q=1")
  expect_error(.normalize_path(NULL))
  expect_error(.normalize_path(NA_character_))
})

test_that("prefix matching handles folder boundaries", {
  expect_true(.prefix_match("/api/v1", "/api"))
  expect_false(.prefix_match("/api-v1", "/api"))
  expect_true(.prefix_match("/", "/"))
})

test_that("websocket event dispatching handles all types", {
  clear_handlers()
  calls <- integer()
  on_ws("open", function(req) calls <<- c(calls, 2L))
  on_ws("message", function(req) calls <<- c(calls, 3L))

  .dispatch_ws_event(list(type = 2L))
  .dispatch_ws_event(list(type = 3L))

  expect_equal(calls, c(2L, 3L))
})

test_that("static files mime guessing covers branches", {
  # Access internal .guess_mime via the environment if not exported
  expect_equal(.guess_mime("test.json"), "application/json")
  expect_equal(.guess_mime("test.css"), "text/css")
  expect_equal(
    .guess_mime("test.unknown"),
    "application/octet-stream"
  )
})

test_that("breadcrumb generation produces valid HTML", {
  res <- .breadcrumb("/api/v1/users")
  expect_match(res, 'href="/api/"', fixed = TRUE)
  expect_match(res, 'href="/api/v1/"', fixed = TRUE)
  expect_match(res, 'href="/api/v1/users/"', fixed = TRUE)
})
