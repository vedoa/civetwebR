#' Start the CivetWeb server
#'
#' @param port Integer. Port to listen on.
#' @param host String. Host to expose to.
#' @param num_threads Integer. Number of worker threads. Default is 50.
#' @return Invisibly returns TRUE.
#' @export
start_server <- function(port = 8080L, host = "127.0.0.1", num_threads = 50L) {
  .validate_is_running()
  .validate_port(port)
  .validate_host(host)
  num_threads <- .validate_num_threads(num_threads)

  ptr <- .Call(
    civetweb_start_server,
    port,
    host,
    num_threads,
    PACKAGE = "civetwebR"
  )

  if (is.null(ptr)) {
    stop("failed to start server", call. = FALSE)
  }

  .state$server_xptr <- ptr
  .set_loop_running(FALSE)

  invisible(TRUE)
}

#' Stop the CivetWeb server
#'
#' @return Invisibly returns TRUE.
#' @export
stop_server <- function() {
  .validate_is_not_running()

  .Call(civetweb_stop_server, .state$server_xptr, PACKAGE = "civetwebR")

  .state$server_xptr <- NULL
  .set_loop_running(FALSE)

  invisible(TRUE)
}
