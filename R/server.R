#' Start the CivetWeb server
#'
#' @param port Integer. Port to listen on.
#' @return Invisibly returns TRUE.
#' @export
start_server <- function(port = 8080L) {
  if (.is_running()) {
    stop("Server is already running", call. = FALSE)
  }

  if (!is.numeric(port) || length(port) != 1L || is.na(port)) {
    stop("port must be a single number", call. = FALSE)
  }

  port <- as.integer(port)
  if (port < 1L || port > 65535L) {
    stop("port must be between 1 and 65535", call. = FALSE)
  }

  ptr <- .Call(civetweb_start_server, port, PACKAGE = "civetwebR")

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
  if (!.is_running()) {
    stop("Server is not running", call. = FALSE)
  }

  if (.is_loop_running()) {
    stop("Cannot stop server while loop is running", call. = FALSE)
  }

  .Call(civetweb_stop_server, .state$server_xptr, PACKAGE = "civetwebR")

  .state$server_xptr <- NULL
  .set_loop_running(FALSE)

  invisible(TRUE)
}