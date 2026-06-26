#' List all pericircle packages
#'
#' @param include_version logical; if `TRUE` (default), returns a named
#'   character vector where names are package names and values are installed
#'   version strings. If `FALSE`, returns a plain character vector of package
#'   names.
#'
#' @return A character vector of package names, optionally named by version.
#'
#' @importFrom utils packageVersion
#' @export
#'
#' @examples
#' pericircle_packages()
#' pericircle_packages(include_version = FALSE)
pericircle_packages <- function(include_version = TRUE) {
  if (!include_version) {
    return(core_packages)
  }

  versions <- vapply(
    core_packages,
    function(p) as.character(utils::packageVersion(p)),
    character(1)
  )

  setNames(versions, core_packages)
}
