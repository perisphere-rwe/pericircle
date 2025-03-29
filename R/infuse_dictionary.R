

#' Infuse data with a dictionary
#'
#' Transfer variable and category labels into a dataframe. This
#'   is useful if you are passing a data frame to a tabulation or plotting
#'   function that naturally incorporates labeled data.
#'
#' @param data a data frame
#'
#' @param dictionary `r roxy_describe_dd()`
#'
#' @param units a character value indicating how units should be incorporated
#'   into variable labels. Valid choices are
#'   - `"none"` : do not include units in labels, e.g., "Age"
#'   - `"descriptive"` : include labels, e.g., "Age, *years*"
#'   - `"model"` : include label and model divisor, e.g., "Age, *per 10 years*"
#'
#' @param warn_undocumented a logical value. If `TRUE` (default), then
#'   a warning is thrown whenever 1 or more variables in `data` do not
#'   have supporting documentation in `dictionary`. If `FALSE`, then
#'   this information will not be presented in a warning.
#'
#' @details
#' Additional details...
#'
#'
#' @export
#'
#' @examples
#' age_years <- numeric_variable(
#'   name = "age_years",
#'   label = "Age of participant",
#'   units = "years",
#'   divby_modeling = 10
#' )
#'
#' gender <- nominal_variable(
#'   name = "gender",
#'   label = "Gender of participant",
#'   category_levels = c("M", "F"),
#'   category_labels = c("Male", "Female")
#' )
#'
#' dd <- data_dictionary(age_years, gender)
#' data <- data.frame(age_years = 55, gender = "M")
#'
#' data %>%
#'   infuse_dictionary(dd, units = "model") %>%
#'   purrr::map(~attr(.x, 'label'))

infuse_dictionary <- function(data,
                              dictionary = NULL,
                              units = "none",
                              warn_undocumented = TRUE){

  checkmate::assert_character(units, null.ok = FALSE, len = 1)
  checkmate::assert_choice(units, choices = c("none", "descriptive", "model"))
  checkmate::assert_logical(warn_undocumented)
  checkmate::assert_data_frame(data)

  dictionary <- dictionary %||% attr(data, 'dictionary')

  if(is.null(dictionary)){
    stop("Data dictionary was not supplied and was not found",
         "in data object", call. = FALSE)
  }

  checkmate::assert_class(dictionary, classes = "DataDictionary")

  overlapping_variables <- names(data) %>%
    intersect(dictionary$get_variable_names())

  undocumented_variables <- names(data) %>%
    setdiff(overlapping_variables)

  if(!is_empty(undocumented_variables) && warn_undocumented){

    msg <- paste0(
      "dictionary does not contain information for some variables in ",
      "data: ", paste(undocumented_variables, collapse = ', '),
      ". To suppress this warning, set `warn_undocumented` to `FALSE`"
    )

    warning(msg, call. = FALSE)

  }

  for(i in overlapping_variables){

    if(dictionary$variables[[i]]$type == "Nominal"){

      .levels <- dictionary$variables[[i]]$category_levels
      .labels <- dictionary$variables[[i]]$category_labels

      data[[i]] <- factor(data[[i]],
                          levels = .levels,
                          labels = .labels %||% .levels)

    }

    if(dictionary$variables[[i]]$type == "Numeric"){

      if(units == 'model') data[[i]] %<>%
          magrittr::divide_by(dictionary$variables[[i]]$divby_modeling %||% 1)

    }

    attr(data[[i]], 'label') <- switch(
      units,
      'none'        = dictionary$variables[[i]]$get_label(),
      'descriptive' = dictionary$variables[[i]]$get_label_and_unit(),
      'model'       = dictionary$variables[[i]]$get_label_divby()
    )

  }

  data

}


