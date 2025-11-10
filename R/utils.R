#' @importFrom rlang inform
inform_startup <- function(msg, ...) {
  if (is.null(msg)) {
    return()
  }

  inform(msg, ..., class = "packageStartupMessage")
}
