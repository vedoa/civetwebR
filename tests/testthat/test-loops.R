# ------------------------------------------------------------------------------
# Server Loops
# ------------------------------------------------------------------------------

test_that("run_server throws error when server not started", {
  # Ensure server is stopped
  if (.is_running()) {
    stop_server()
  }
  expect_error(run_server(), "Server is not running")
})

test_that("run_server prevents double-looping", {
  # satisfied .is_running() check by mocking the pointer
  .state$server_xptr <- TRUE
  on.exit(.state$server_xptr <- NULL)

  # Mock the loop already being active
  .set_loop_running(TRUE)
  on.exit(.set_loop_running(FALSE), add = TRUE)
  expect_error(run_server(), "Server loop is already running")
})
