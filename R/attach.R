core_packages <- c(
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
