#' Start and run the HTTP server (blocking)
#'
#' @param port Integer. Port to listen on.
#' @return Never returns (runs until interrupted).
#' @export
serve <- function(port = 8080L) {
  start_server(port)

  on.exit(
    {
      if (.is_running()) {
        try(stop_server(), silent = TRUE)
      }
    },
    add = TRUE
  )

  run_server()
}
