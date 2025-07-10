

#' Get Unknown Fields in Dictionary
#'
#' @param x `r roxy_describe_dd()`
#'
#' @param as_request logical. If `TRUE`, text is returned in a readable
#'   format that can be pasted into other settings, e.g., an e-mail
#'   requesting for the unknown information. If `FALSE` (the default),
#'   unknowns are returned as a `tibble`.
#'
#' @return If `as_request = FALSE`, a [tibble][tibble::tibble-package].
#'   If `as_request = TRUE`, text is returned describing the unknowns.
#'
#' @export
#'
#' @examples
#'
#' # units isn't returned in the output because it is specified (known)
#' get_unknowns(data_dictionary(numeric_variable("a", units = 'years')))
#'
#' # label isn't returned in the output because it is specified (known)
#' get_unknowns(data_dictionary(nominal_variable("b", label = 'example')))
#' # as_request = TRUE returns this information in a shareable format
#' as_data_dictionary(iris) %>%
#'   set_labels(Sepal.Length = "Sepal length",
#'             Sepal.Width = "Sepal width",
#'             Species = "Flower species") %>%
#'   set_descriptions(Species = "The species are all subtypes of iris") %>%
#'   set_units(Sepal.Length = "cm") %>%
#'   get_unknowns(as_request = TRUE)

get_unknowns <- function(x, as_request = FALSE, show_optional = FALSE){

  data_unknowns <- x$dictionary %>%
    dplyr::mutate(type = purrr::map_chr(x$variables, "type"), .after = 1) %>%
    dplyr::rename(variable = name) %>%
    dplyr::mutate(
      category_labels = dplyr::if_else(category_labels==category_levels,
                                       true = 'none',
                                       false = category_labels)
    ) %>%
    tidyr::pivot_longer(cols = -c(variable, type)) %>%
    dplyr::filter(
      dplyr::case_when(
        name %in% c("label", "description") ~ value == 'none',
        type == "Numeric" ~ value == "none" & name %in% c("units",
                                                          "divby_modeling"),
        type == "Nominal" ~ value == "none" & name %in% c("category_levels",
                                                          "category_labels")
      )
    ) %>%
    dplyr::mutate(name = factor(name,
                                levels = c("label", "category_labels",
                                           "units", "divby_modeling",
                                           "description"))) %>%
    dplyr::arrange(name)

  if(!show_optional){
    data_unknowns <- data_unknowns %>%
      dplyr::filter(name %in% c("label", "units", "category_labels")) %>%
      droplevels()
  }

  if(!as_request) return(data_unknowns)

  split(data_unknowns, data_unknowns$name) %>%
    purrr::map_chr(
      .f = ~ {

        .x_name <- switch(
          as.character(.x$name[1]),
          label = "A label to use for this variable in reports",
          description = paste(
            "Optional: additional relevant details",
            "(e.g., method of collection or measurement)"
          ),
          units = "Variable units (e.g., age in years)",
          divby_modeling = paste(
            "Units for model output",
            "(e.g., per 10 years of age)"
          ),
          category_levels = paste(
            "Category levels for this variable",
            "(levels are the values for this variable in data)"
          ),
          category_labels = paste(
            "Category labels for this variable",
            "(labels are shown in reports)"
          )
        )

        text_mid <- ' = ?'

        if(.x$name[1] == 'category_labels'){

          text_mid <- purrr::map_chr(
            .x = .x$variable,
            .f = function(..x){
              paste(x$variables[[..x]]$category_levels,
                    '= ?',
                    collapse = ";  ")
            }
          ) %>%
            paste0(": ", .)

        }

        paste0(.x_name, ":", "\n\n",
               paste0("  - ", .x$variable, text_mid, collapse = '\n'))

      }
    ) %>%
    paste(collapse = "\n\n") %>%
    cat("\n")

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
#' age_years <- numeric_variable(
#'   name = "age_years",
#'   label = "Age of participant",
#'   units = 'years'
#' )
#'
#' age_group <- nominal_variable(
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


get_terms_key <- function(x, terms_sep = ""){

  purrr::map(x$variables, ~.x$fetch_category_levels() %||% "..none..") %>%
    tibble::enframe(name = 'variable', value = 'levels') %>%
    tidyr::unnest(cols = levels) %>%
    dplyr::mutate(
      levels = dplyr::if_else(levels == '..none..',
                              NA_character_,
                              levels),
      term = dplyr::if_else(is.na(levels),
                            variable,
                            paste(variable, levels, sep = terms_sep)),
      .before = variable
    )

}


