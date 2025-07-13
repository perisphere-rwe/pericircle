

#' Bind Dictionaries Together
#'
#' Binding dictionaries together can be useful when you have variables that
#'   exist in multiple datasets and you don't want to re-write their meta
#'   information into each of those datasets.
#'
#' @param x,y `r roxy_describe_dd()`
#' @param conflict_preference a character value indicating what to do
#'   when `x` and `y` have overlapping variables. If "left", then
#'   the definitions in the `x` dictionary are preferred because
#'   `x` is on the left of `y` in the function arguments. If "right",
#'   `y` definitions are preferred.
#'
#' @returns `r roxy_describe_dd()`
#'
#' @export
#'
#' @examples
#'
#' dd_age_years <- data_dictionary(
#'   numeric_variable(
#'     name = "age_years",
#'     label = "Age",
#'     units = 'years',
#'     divby_modeling = 10
#'   )
#' )
#'
#' dd_age_group <- data_dictionary(
#'   nominal_variable(
#'     name = "age_group",
#'     label = "Age group",
#'     description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
#'     category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
#'     category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
#'   )
#' )
#'
#' bind_dictionary(dd_age_years, dd_age_group)

bind_dictionary <- function(x, y, conflict_preference = NULL) {

  stopifnot(inherits(x, "DataDictionary"))
  stopifnot(inherits(y, "DataDictionary"))

  vars1 <- x$variables
  vars2 <- y$variables

  dupes <- intersect(names(vars1), names(vars2))

  if (!purrr::is_empty(dupes) && is.null(conflict_preference)) {
    warning("Overlapping variable names in dictionaries:\n\n",
            paste(paste("-",dupes), collapse = "\n"),
            "\n\nVariable definitions from the `x` dictionary are ",
            "preferred by default. Use `conflict_preference` to ",
            "modify this pattern (and to quiet this warning message).",
            call. = FALSE)
  }

  combined_vars <- switch(
    conflict_preference %||% "left",
    left  = c(vars1, vars2[setdiff(names(vars2), names(vars1))]),
    right = c(vars1[setdiff(names(vars1), names(vars2))], vars2),
  ) %>%
    # keep the order of the x dictionary,
    .[c(names(vars1), setdiff(names(vars2), names(vars1)))]

  DataDictionary$new(combined_vars)

}
