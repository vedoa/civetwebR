test_that("static: single mount works", {
  tmp <- tempdir()

  public <- file.path(tmp, "public")
  dir.create(public, showWarnings = FALSE)

  writeLines("hello world", file.path(public, "hello.txt"))
  writeLines("<h1>index</h1>", file.path(public, "index.html"))

  static <- .validate_static(list(
    dir = public,
    prefix = "/"
  ))

  h <- static_files(
    dir = static[[1]]$dir,
    prefix = static[[1]]$prefix,
    index = static[[1]]$index
  )

  # file
  res <- h(list(path = "/hello.txt"))
  expect_equal(res$status, 200L)
  expect_match(rawToChar(res$body), "hello world")

  # index
  res <- h(list(path = "/"))
  expect_equal(res$status, 200L)
  expect_match(rawToChar(res$body), "index")

  # 404
  res <- h(list(path = "/missing.txt"))
  expect_equal(res$status, 404L)
})


test_that("static: multiple mounts auto-sort correctly", {
  tmp <- tempdir()

  public <- file.path(tmp, "public")
  assets <- file.path(tmp, "assets")

  dir.create(public, showWarnings = FALSE)
  dir.create(assets, showWarnings = FALSE)

  writeLines("root", file.path(public, "file.txt"))
  writeLines("asset", file.path(assets, "file.txt"))

  static <- .validate_static(list(
    list(dir = public, prefix = "/"),
    list(dir = assets, prefix = "/assets")
  ))

  handlers <- lapply(static, function(s) {
    static_files(
      dir = s$dir,
      prefix = s$prefix,
      index = s$index
    )
  })

  req <- list(path = "/assets/file.txt")

  res <- NULL
  for (i in seq_along(handlers)) {
    if (startsWith(req$path, static[[i]]$prefix)) {
      res <- handlers[[i]](req)
      break
    }
  }

  # must come from /assets, not /
  expect_equal(res$status, 200L)
  expect_match(rawToChar(res$body), "asset")
})


test_that("static: directory traversal is blocked", {
  tmp <- tempdir()

  public <- file.path(tmp, "public")
  dir.create(public, showWarnings = FALSE)

  writeLines("secret", file.path(tmp, "secret.txt"))

  static <- .validate_static(list(
    dir = public,
    prefix = "/"
  ))

  h <- static_files(
    dir = static[[1]]$dir,
    prefix = static[[1]]$prefix,
    index = static[[1]]$index
  )

  res <- h(list(path = "/../secret.txt"))

  expect_equal(res$status, 403L)
})


test_that("static: index fallback works", {
  tmp <- tempdir()

  public <- file.path(tmp, "public")
  dir.create(public, showWarnings = FALSE)

  writeLines("alt index", file.path(public, "default.html"))

  static <- .validate_static(list(
    dir = public,
    prefix = "/",
    index = c("default.html", "index.html")
  ))

  h <- static_files(
    dir = static[[1]]$dir,
    prefix = static[[1]]$prefix,
    index = static[[1]]$index
  )

  res <- h(list(path = "/"))

  expect_equal(res$status, 200L)
  expect_match(rawToChar(res$body), "alt index")
})

test_that("integration: static end-to-end", {
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
  pkg_path <- normalizePath(test_path("../.."), mustWork = TRUE)

  p <- callr::r_bg(
    function(tmp, port, lib_paths, pkg_path) {
      .libPaths(lib_paths)
      if (file.exists(file.path(pkg_path, "DESCRIPTION"))) {
        pkgload::load_all(pkg_path)
      } else {
        library(civetwebR)
      }

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
    args = list(tmp, port, .libPaths(), pkg_path)
  )

  on.exit(
    {
      if (p$is_alive()) p$kill()
    },
    add = TRUE
  )

  # wait_for_server is defined in test-integration.R but test_bg helpers 
  # should be robust. Re-using the reliable port-based check:
  wait_for_server(port, p)

  GET <- function(path) {
    con <- url(paste0(base, path))
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE), collapse = "\n")
  }

  expect_match(GET("/file.txt"), "root-file", fixed = TRUE)
  expect_match(GET("/assets/file.txt"), "asset-file", fixed = TRUE)
  expect_match(GET("/"), "INDEX", fixed = TRUE)
  expect_error(GET("/does-not-exist"))
})
