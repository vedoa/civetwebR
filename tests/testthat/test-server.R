test_that("server starts and updates state", {
  if (!is.null(.state$server_xptr)) {
    stop_server()
  }

  start_server(8080L)

  expect_false(is.null(.state$server_xptr))
})

test_that("server stops and clears state", {
  if (!is.null(.state$server_xptr)) {
    stop_server()
  }

  start_server(8080L)
  stop_server()

  expect_null(.state$server_xptr)
})

test_that("cannot start server twice", {
  if (!is.null(.state$server_xptr)) {
    stop_server()
  }

  start_server(8080L)

  expect_error(start_server(8080L))

  stop_server()
})

test_that("cannot stop server if not running", {
  if (!is.null(.state$server_xptr)) {
    stop_server()
  }

  expect_error(stop_server())
})
