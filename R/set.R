

#' Modify Data Dictionary Elements
#'
#' @param x `r roxy_describe_dd()`
#' @param ... Name-value pairs. The name gives the name of the variable
#'   that will be modified. The value must be a:
#'
#'   - character value for `set_label()`, `set_units()`, and `set_description()`
#'   - numeric value for `set_divby_modeling()`
#'   - character vector for `set_category_levels()` and `set_category_labels()`
#'
#' @return a modified `x`
#'
#' @export
#'
#' @examples
#'
#' dd <- as_data_dictionary(data.frame(a = 1, b = "cat")) %>%
#'  set_label(a = "example", b = "categorical example") %>%
#'  set_units(a = "years") %>%
#'  set_description(a = "A variable used for examples") %>%
#'  set_category_labels(b = "A small lion")
#'
#' dd
#'

set_label <- function(x, ...){
  x$modify_dictionary(list(...), field = 'label')
  x
}

#' @rdname set_label
#' @export
set_description <- function(x, ...){
  x$modify_dictionary(list(...), field = 'description')
  x
}

#' @rdname set_label
#' @export
set_units <- function(x, ...){
  x$modify_dictionary(list(...), field = 'units')
  x
}

#' @rdname set_label
#' @export
set_divby_modeling <- function(x, ...){
  x$modify_dictionary(list(...), field = 'divby_modeling')
  x
}

#' @rdname set_label
#' @export
set_category_levels <- function(x, ...){
  x$modify_dictionary(list(...), field = 'category_levels')
  x
}

#' @rdname set_label
#' @export
set_category_labels <- function(x, ...){
  x$modify_dictionary(list(...), field = 'category_labels')
  x
}

