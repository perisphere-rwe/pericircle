

#' Title
#'
#' @param x
#' @param as_request
#'
#' @return
#' @export
#'
#' @examples
get_unknowns <- function(x, as_request = FALSE){

  data_unknowns <- x$dictionary %>%
    dplyr::mutate(type = purrr::map_chr(x$variables, "type"), .after = 1) %>%
    dplyr::rename(variable = name) %>%
    tidyr::pivot_longer(cols = -c(variable, type)) %>%
    dplyr::filter(
      dplyr::case_when(
        name %in% c("label", "description") ~ value == 'none',
        type == "Numeric" ~ value == "none" & name %in% c("units",
                                                          "divby_modeling"),
        type == "Nominal" ~ value == "none" & name %in% c("category_levels",
                                                          "category_labels")
      )
    )

  if(!as_request) return(data_unknowns)

  'ok'

}

#' Get data dictionary as a data frame
#'
#' @param x a `DataDictionary` object
#'
#' @return a `tibble` containing the data dictionary information
#'
#' @export
#'
#' @examples
#'
#' age_years <- NumericVariable$new(
#'   name = "age",
#'   label = "Age of participant"
#' )
#'
#' age_group <- NominalVariable$new(
#'   name = "age_group",
#'   label = "Age group",
#'   category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
#'   category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
#' )
#'
#' dd <- data_dictionary(age_years, age_group)
#'
#' get_dictionary(dd)
#'
get_dictionary <- function(x){
  x$dictionary
}
