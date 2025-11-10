inform_startup <- function(msg, ...) {
  if (is.null(msg)) {
    return()
  }

  rlang::inform(msg, ..., class = "packageStartupMessage")
}
