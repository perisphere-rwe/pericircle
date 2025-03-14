

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
set_label <- function(x, ...){
  x$modify_dictionary(list(...), field = 'label')
  x
}

#' @rdname set_label
set_units <- function(x, ...){
  x$modify_dictionary(list(...), field = 'units')
  x
}

#' @rdname set_label
set_description <- function(x, ...){
  x$modify_dictionary(list(...), field = 'description')
  x
}

#' @rdname set_label
set_units <- function(x, ...){
  x$modify_dictionary(list(...), field = 'units')
  x
}

#' @rdname set_label
set_divby_modeling <- function(x, ...){
  x$modify_dictionary(list(...), field = 'divby_modeling')
  x
}

#' @rdname set_label
set_category_levels <- function(x, ...){
  x$modify_dictionary(list(...), field = 'category_levels')
  x
}

#' @rdname set_label
set_category_labels <- function(x, ...){
  x$modify_dictionary(list(...), field = 'category_labels')
  x
}

