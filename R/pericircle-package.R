#' @keywords internal
#' @import R6
"_PACKAGE"

utils::globalVariables(names = c("name", "variable", "type",
                                 ".data", ".group_level", ":="))

## usethis namespace: start
#' @importFrom dplyr .data
#' @importFrom magrittr %<>%
#' @importFrom purrr is_empty
#' @importFrom Rcpp sourceCpp
#' @importFrom tibble tibble
#' @import data.table
#' @useDynLib pericircle, .registration = TRUE
## usethis namespace: end
NULL
