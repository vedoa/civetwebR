# ------------------------------------------------------------------------------
# High-level Serve API
# ------------------------------------------------------------------------------

test_that("serve validation works", {
  expect_error(serve(port = NA), "port")
  expect_error(serve(port = 99999), "port")
  expect_error(serve(host = NA), "host")
  expect_error(serve(host = ""), "host")
  expect_error(serve(quiet = NA), "quiet")
  expect_error(serve(log = 123), "log")
})
