# Code taken from https://github.com/tidyverse/tidyverse and modified
core_packages <- c(
  "peridefs",
  "periglue",
  "pericircumference",
  "perinary"
)

# Vector of core packages that are not attached
core_unloaded <- function() {
  search <- paste0("package:", core_packages)

  return(
    core_packages[!search %in% search()]
  )
}


pericircle_attach <- function() {
  to_load <- core_unloaded()

  suppressPackageStartupMessages(
    lapply(to_load, function(pkg) {
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    })
  )

  return(
    invisible(to_load)
  )
}

#' @title Attach message with version numbers of each pericircle package
#'
#' @param to_load character; one or more names of packages to load.
#'
#' @importFrom cli rule style_bold col_green col_blue symbol ansi_align ansi_nchar
#'
#' @noRd
pericircle_attach_message <- function(to_load) {
  if (length(to_load) == 0) {
    return(NULL)
  }

  header <- rule(
    left = style_bold("Attaching core pericircle packages"),
    right = paste0("pericircle ", package_version_h("pericircle"))
  )

  to_load <- sort(to_load)
  versions <- vapply(to_load, package_version_h, character(1))

  packages <- paste0(
    col_green(symbol$tick),
    " ",
    col_blue(format(to_load)),
    " ",
    ansi_align(versions, max(ansi_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }

  ncols <- 2
  col1 <- seq_len(length(packages) / ncols)
  info <- paste0(packages[col1], "     ", packages[-col1])

  paste0(header, "\n", paste(info, collapse = "\n"))
}

#' @importFrom utils packageVersion
package_version_h <- function(pkg) {
  highlight_version(utils::packageVersion(pkg))
}

#' @importFrom cli col_red
highlight_version <- function(x) {
  x <- as.character(x)

  is_dev <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    !is.na(x) & x >= 9000
  }

  pieces <- strsplit(x, ".", fixed = TRUE)
  pieces <- lapply(pieces, function(x) ifelse(is_dev(x), col_red(x), x))
  vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1))
}
