

#' Modify Data Dictionary Elements
#'
#' @param dictionary `r roxy_describe_dd()`
#' @param ... Name-value pairs. The name gives the name of the variable
#'   that will be modified. The value must be a:
#'
#'   - character value for `set_labels()`, `set_units()`,
#'     and `set_descriptions()`
#'   - numeric value for `set_divby_modeling()`
#'   - character vector for `set_category_levels()` and `set_category_labels()`
#'
#' @return a modified `dictionary`
#'
#' @export
#'
#' @examples
#'
#' dd <- as_data_dictionary(data.frame(a = 1, b = "cat")) %>%
#'  set_labels(a = "example", b = "categorical example") %>%
#'  set_units(a = "years") %>%
#'  set_descriptions(a = "A variable used for examples") %>%
#'  set_category_labels(b = "A small lion")
#'
#' dd
#'

set_labels <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$modify_dictionary(list(...), field = 'label')
  dictionary
}

#' @rdname set_labels
#' @export
set_descriptions <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$modify_dictionary(list(...), field = 'description')
  dictionary
}

#' @rdname set_labels
#' @export
set_units <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$modify_dictionary(list(...), field = 'units')
  dictionary
}

#' @rdname set_labels
#' @export
set_divby_modeling <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$modify_dictionary(list(...), field = 'divby_modeling')
  dictionary
}

#' @rdname set_labels
#' @export
set_category_levels <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$modify_dictionary(list(...), field = 'category_levels')
  dictionary
}

#' @rdname set_labels
#' @export
set_category_labels <- function(dictionary, ...){
  checkmate::assert_class(dictionary, "DataDictionary")
  dictionary$modify_dictionary(list(...), field = 'category_labels')
  dictionary
}


#' @rdname set_labels
#' @export
set_factor_labels <- function(dictionary, ...){
  set_factors(dictionary, ..., .relevel = FALSE)
}


#' @rdname set_labels
#' @export
set_factor_order <- function(dictionary, ...){

  checkmate::assert_class(dictionary, "DataDictionary")

  .dots <- list(...)

  for(i in names(.dots)){

    # expects input of the form: name = c(level = label).
    checkmate::assert_character(i, .var.name = i, null.ok = FALSE)

    # Here, i is the name, so it should match an existing name in dictionary
    checkmate::assert_choice(i,
                             .var.name = i,
                             choices = dictionary$get_variable_names())

    if(!is.null(names(.dots[[i]]))){
      stop("Vector inputs to `set_factor_levels` must not be named.\n",
           " - x = c(\"new_first\", \"new_second\") works\n",
           " - x = c(a = \"new_first\", b = \"new_second\") does not work.\n",
           "See ?set_factors to re-order levels and modify labels simultaneously",
           call. = FALSE)
    }

    # create input lists for eventual call to modify_dictionary
    inputs <- purrr::set_names(list(as.character(.dots[[i]])), i)

    current_lvls <- dictionary$variables[[i]]$get_category_levels()

    # user inputs must be character valued and match existing
    # levels in the dictionary for this variable
    for(j in inputs[[1]]){
      checkmate::assert_character(j, .var.name = j, null.ok = FALSE)
      checkmate::assert_choice(j, .var.name = j, choices = current_lvls)
    }

    unused_lvls <- setdiff(current_lvls, inputs[[1]])

    inputs[[1]] %<>% append(unused_lvls)

    dictionary$modify_dictionary(inputs, field = 'category_levels')

    if(!is.null(dictionary$variables[[i]]$category_labels)){

      current_labs <- dictionary$variables[[i]]$get_category_labels() %>%
        purrr::set_names(current_lvls)

      input_labs <- inputs
      input_labs[[1]] <- as.character(current_labs[inputs[[1]]])
      dictionary$modify_dictionary(input_labs, field = 'category_labels')

    }

  }

  dictionary

}



set_factors <- function(dictionary, ..., .relevel){

  checkmate::assert_class(dictionary, "DataDictionary")

  .dots <- list(...)

  for(i in names(.dots)){

    # expects input of the form: name = c(level = label).
    checkmate::assert_character(i, .var.name = i, null.ok = FALSE)

    # Here, i is the name, so it should match an existing name in dictionary
    checkmate::assert_choice(i,
                             .var.name = i,
                             choices = dictionary$get_variable_names())

    # create input lists for eventual call to modify_dictionary
    input_lvls <- purrr::set_names(list(names(.dots[[i]])), i)
    input_labs <- purrr::set_names(list(as.character(.dots[[i]])), i)

    if(is.null(input_lvls[[1]])) stop(
      "Inputs to `set_factors` must be named.\n",
      " - x = c(old = \"new\") works\n",
      " - x = \"new\" does not work.\n",
      "See ?set_factor_levels to re-order levels without modifying labels",
      call. = FALSE
    )

    current_lvls <- dictionary$variables[[i]]$get_category_levels()
    current_labs <- dictionary$variables[[i]]$fetch_category_labels()

    # user inputs must be character valued and match existing
    # levels in the dictionary for this variable
    for(j in input_lvls[[1]]){
      checkmate::assert_character(j, .var.name = j, null.ok = FALSE)
      checkmate::assert_choice(j, .var.name = j, choices = current_lvls)
    }

    # modify both the labels and the order of the levels
    if(.relevel){

      unused_lvls <- setdiff(current_lvls, input_lvls[[1]])
      unused_labs <- current_labs[current_lvls %in% unused_lvls]

      input_lvls[[1]] %<>% append(unused_lvls)
      input_labs[[1]] %<>% append(unused_labs)

      dictionary$modify_dictionary(input_lvls, field = 'category_levels')
      dictionary$modify_dictionary(input_labs, field = 'category_labels')

    } else {

      # put the inputs into the existing labels.
      input_key <- purrr::set_names(input_labs[[1]], input_lvls[[1]])

      # be careful not to mess up the established order
      updated_key <- purrr::set_names(current_labs, current_lvls)
      updated_key[names(input_key)] <- input_key

      input_labs[[1]] <- purrr::set_names(updated_key, NULL)

      dictionary$modify_dictionary(input_labs, field = 'category_labels')

    }

  }

  dictionary

}

