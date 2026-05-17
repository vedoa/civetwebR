test_that("port validation works", {
  expect_equal(.validate_port(8080), 8080L)
  expect_error(.validate_port(-1))
  expect_error(.validate_port(70000))
  expect_error(.validate_port("abc"))
})

test_that("host validation works", {
  expect_equal(.validate_host("127.0.0.1"), "127.0.0.1")
  expect_equal(.validate_host("localhost"), "localhost")
  expect_equal(.validate_host("::1"), "[::1]")
  expect_error(.validate_host(""))
  expect_error(.validate_host("invalid host!"))
})

test_that("timeout_ms validation works", {
  expect_equal(.validate_timeout_ms(100), 100L)
  expect_equal(.validate_timeout_ms(0), 0L)
  expect_error(.validate_timeout_ms(-10))
  expect_error(.validate_timeout_ms("100"))
})

test_that("num_threads validation works", {
  expect_equal(.validate_num_threads(5), 5L)
  expect_error(.validate_num_threads(0))
  expect_error(.validate_num_threads(-1))
})

test_that("max_body_size validation works", {
  expect_equal(.validate_max_body_size(1024), 1024)
  expect_equal(.validate_max_body_size(1e6), 1e6)
  expect_error(.validate_max_body_size(-1))
  expect_error(.validate_max_body_size(NA))
})

test_that("request_timeout_ms validation works", {
  expect_equal(.validate_request_timeout_ms(30000), 30000L)
  expect_error(.validate_request_timeout_ms(-1))
})

test_that("serve input aggregator works", {
  args <- .validate_serve_input(
    8080,
    "127.0.0.1",
    TRUE,
    NULL,
    100,
    5,
    1024,
    5000
  )
  expect_equal(args$port, 8080L)
  expect_equal(args$num_threads, 5L)
  expect_equal(args$max_body_size, 1024)
  expect_equal(args$request_timeout_ms, 5000L)
})

test_that("host IPv6 normalization works", {
  expect_equal(.validate_host("::1"), "[::1]")
  expect_equal(.validate_host("[::1]"), "[::1]")
  expect_error(.validate_host("[invalid]"))
})

test_that("static validation handles single config", {
  cfg <- list(dir = tempdir(), prefix = "/test")
  expect_length(.validate_static(cfg), 1)
})
