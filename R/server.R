#' Start the CivetWeb server
#'
#' @param port Integer. Port to listen on.
#' @param host String. Host to expose to.
#' @return Invisibly returns TRUE.
#' @export
start_server <- function(port = 8080L, host = "127.0.0.1") {
  .validate_is_running()
  .validate_port(port)
  .validate_host(host)

  ptr <- .Call(civetweb_start_server, port, host, PACKAGE = "civetwebR")

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
