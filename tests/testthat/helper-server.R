wait_for_server <- function(port, p = NULL, timeout = 5) {
  start <- Sys.time()

  repeat {
    if (!is.null(p) && !p$is_alive()) {
      stop("Server crashed:\n", p$read_error())
    }

    ok <- tryCatch(
      {
        con <- socketConnection(
          host = "127.0.0.1",
          port = port,
          open = "r+",
          blocking = TRUE
        )
        close(con)
        TRUE
      },
      error = function(e) FALSE
    )

    if (ok) {
      return(TRUE)
    }

    if (as.numeric(Sys.time() - start, units = "secs") > timeout) {
      stop("Server did not start")
    }

    Sys.sleep(0.05)
  }
}