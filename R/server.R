#' Start the CivetWeb server
#'
#' @param port Integer. Port to listen on.
#' @param host String. Host to expose to.
#' @param num_threads Integer. Number of worker threads. Default is 50.
#' @param max_body_size Number. Maximum request body size in bytes. Default is 8MiB.
#' @param request_timeout_ms Integer. Socket receive timeout. Default is 30s.
#' @return Invisibly returns TRUE.
#' @export
start_server <- function(
  port = 8080L, 
  host = "127.0.0.1", 
  num_threads = 50L,
  max_body_size = 8 * 1024 * 1024,
  request_timeout_ms = 30000L
) {
  .validate_is_running()
  .validate_port(port)
  .validate_host(host)
  num_threads <- .validate_num_threads(num_threads)
  max_body_size <- .validate_max_body_size(max_body_size)
  request_timeout_ms <- .validate_request_timeout_ms(request_timeout_ms)

  ptr <- .Call(
    civetweb_start_server,
    port,
    host,
    num_threads,
    max_body_size,
    request_timeout_ms,
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
