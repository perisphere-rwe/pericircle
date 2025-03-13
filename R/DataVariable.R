
# DataVariable ----

DataVariable <- R6::R6Class(

  "DataVariable",

  public = list(

    # relevant for all variables
    name = NULL,
    type = NULL,
    label = NULL,
    description = NULL,

    # only relevant for nominal variables but should be
    # set to a value of 'none' for numeric variables
    category_levels = NULL,
    category_labels = NULL,

    # only relevant for numeric variables but should be
    # set to a value of 'none' for nominal variables
    units = NULL,
    divby_modeling = NULL,

    # Retrievers
    get_element = function(x){
      if(is.null(self[[x]])) return("none")
      self[[x]]
    },

    fmt_element = function(x, collapse = ', '){
      paste(self$get_element(x), collapse = collapse)
    },

    set_element = function(field, value){
      self[[field]] <- value
    },

    set_name = function(value) self$set_element("name", value),
    set_type = function(value) self$set_element("type", value),
    set_label = function(value) self$set_element("label", value),
    set_description = function(value) self$set_element("description", value),
    set_units = function(value) self$set_element("units", value),
    set_divby_modeling = function(value) self$set_element("divby_modeling", value),
    set_category_levels = function(value) self$set_element("category_levels", value),
    set_category_labels = function(value) self$set_element("category_labels", value),

    get_name = function() self$get_element("name"),
    get_type = function() self$get_element("type"),
    get_label = function() self$get_element("label"),
    get_description = function() self$get_element("description"),
    get_units = function() self$get_element("units"),
    get_divby_modeling = function() self$get_element("divby_modeling"),
    get_category_levels = function() self$get_element("category_levels"),
    get_category_labels = function() self$get_element("category_labels"),

    fmt_name = function() self$fmt_element("name"),
    fmt_type = function() self$fmt_element("type"),
    fmt_label = function() self$fmt_element("label"),
    fmt_description = function() self$fmt_element("description"),
    fmt_units = function() self$fmt_element("units"),
    fmt_divby_modeling = function() self$fmt_element("divby_modeling"),
    fmt_category_levels = function() self$fmt_element("category_levels"),
    fmt_category_labels = function() self$fmt_element("category_labels"),

    # Constructor
    initialize = function(name,
                          type = "Data",
                          label = NULL,
                          description = NULL) {

      if (missing(name) || !is.character(name) || length(name) != 1) {
        stop("'name' must be a single character string and is required.",
             call. = FALSE)
      }

      self$name        <- name
      self$type        <- type
      self$label       <- label
      self$description <- description


    },

    # Method to print object information
    print = function(...) {
      cat(self$fmt_type(), "Variable:\n")
      cat("  Name               :", self$fmt_name(),        "\n")
      cat("  Label              :", self$fmt_label(),       "\n")
      cat("  Description        :", self$fmt_description(), "\n")
    }
  ),

  private = list(

  )

)

# NumericVariable ----

NumericVariable <- R6::R6Class(

  "NumericVariable",

  inherit = DataVariable,  # Inherit from DataVariable

  public = list(

    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          units = NULL,
                          divby_modeling = NULL) {

      # Call the parent class (DataVariable) initialize
      super$initialize(
        name = name,
        type = "Numeric",
        label = label,
        description = description
      )

      # Set numeric fields
      self$units          <- units
      self$divby_modeling <- divby_modeling

    },

    # Overriding print method to include unit information
    print = function(...) {
      super$print(...)          # Call the parent class print
      cat("  Units              :", self$fmt_units(), "\n")
      cat("  Modeling Divisor   :", self$fmt_divby_modeling(), "\n")
    }
  )
)

# NominalVariable ----

NominalVariable <- R6::R6Class(
  "NominalVariable",
  inherit = DataVariable,  # Inherit from DataVariable

  public = list(

    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          category_levels = NULL,
                          category_labels = NULL) {

      # Call parent class initialize
      super$initialize(
        name = name,
        type = "Nominal",
        label = label,
        description = description
      )

      # Ensure category_levels are provided and are unique
      if (!is.null(category_levels)) {

        if (any(duplicated(category_levels))) {
          stop("'category_levels' must be unique", call. = FALSE)
        }

        self$category_levels <- category_levels

      }

      # If category_labels are not provided, default them to category_levels
      if (is.null(category_labels) && !is.null(category_levels)) {

        self$category_labels <- as.character(category_levels)

      } else if (!is.null(category_labels)) {

        # If both provided, ensure lengths match
        if (length(category_labels) != length(category_levels)) {

          stop("'category_labels' must be the",
               "same length as 'category_levels'.",
               call. = FALSE)

        }

        self$category_labels <- category_labels

      }

      # Set nominal fields
      self$category_levels <- category_levels
      self$category_labels <- category_labels


    },

    # Overriding print method to include category information
    print = function(...) {

      super$print(...)  # Call the parent class print

      cat("  Category Levels    :", self$fmt_category_levels(), "\n")
      cat("  Category Labels    :", self$fmt_category_labels(), "\n")

    }
  )
)

# DataDictionary ----

DataDictionary <- R6::R6Class(

  "DataDictionary",

  public = list(

    # list of numeric and/or nominal variables
    variables = NULL,

    # data frame summary
    dictionary = NULL,

    # Constructor
    initialize = function(vars) {

      # Validate that all are instances of DataVariable or its children
      if (length(vars) == 0) {
        stop("At least one variable must be provided ",
             "to create a DataDictionary.", call. = FALSE)
      }

      if (!all(purrr::map_lgl(vars, ~ inherits(.x, "DataVariable")))) {
        stop("All inputs must inherit from 'DataVariable' ",
             "(e.g., NumericVariable, NominalVariable).", call. = FALSE)
      }

      var_names <- purrr::map_chr(vars, ~.x$name)

      # Store variables list
      self$variables <- purrr::set_names(vars, var_names)

      # Extract data for the tibble
      self$dictionary <- self$create_dictionary(vars)

    },

    # Function to create tibble summary of variables
    create_dictionary = function(vars) {

      tibble::tibble(
        name            = purrr::map_chr(vars, ~ .x$fmt_name()),
        label           = purrr::map_chr(vars, ~ .x$fmt_label()),
        description     = purrr::map_chr(vars, ~ .x$fmt_description()),
        units           = purrr::map_chr(vars, ~ .x$fmt_units()),
        divby_modeling  = purrr::map_chr(vars, ~ .x$fmt_divby_modeling()),
        category_levels = purrr::map_chr(vars, ~ .x$fmt_category_levels()),
        category_labels = purrr::map_chr(vars, ~ .x$fmt_category_levels())
      )

    },

    modify_dictionary = function(..., field){

      .dots <- list(...)

      browser()

      for(i in names(.dots)){
        self$variables[[i]]$set_element(field = field, value = .dots[[i]])
      }

      # Extract data for the tibble
      self$dictionary <- self$create_dictionary(self$variables)

    },

    # Print method
    print = function(...) {

      cat("Data Dictionary:\n")
      print(self$dictionary, n = Inf)

    }
  ),

  private = list(


  )

)


#' Coerce data dictionary to data frame
#'
#' @param x a `DataDictionary` object
#' @param ...
#'
#' @return a `tibble`
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
#' data_dictionary(age_years, age_group)

as_tibble.DataDictionary <- function(x, ...){
  x$dictionary
}


#' Create Data Dictionary from Variable Definitions
#'
#' Initializes a `DataDictionary` object from a set of variables.
#' Variables should be made using `numeric_variable()` or `nominal_variable()`
#' and passed directly as arguments.
#'
#' @param ... One or more objects inheriting from `NumericVariable`
#'   or `NominalVariable`.
#'
#' @return A `DataDictionary` object containing a tibble summary of all
#'   variables.
#'
#' @export
#'
#' @examples
#'
#' age_years <- numeric_variable(
#'   name = "age",
#'   label = "Age of participant",
#'   units = "years"
#' )
#'
#' gender <- nominal_variable(
#'   name = "gender",
#'   label = "Gender of participant",
#'   category_levels = c("M", "F"),
#'   category_labels = c("Male", "Female")
#' )
#'
#' dict <- data_dictionary(age_years, gender)
#' print(dict)
#'
data_dictionary <- function(...){

  DataDictionary$new(list(...))

}


#' Create Data Dictionary from Existing Data
#'
#' @param x a data frame
#'
#' @return a `DataDictionary`
#'
#' @export
#'
#' @examples
#'
as_data_dictionary <- function(x){

  stopifnot(is.data.frame(x))

  vars <- purrr::map2(
    .x = x,
    .y = names(x),
    .f = ~ switch(
      class(.x)[1],
      'factor' = NominalVariable$new(name = .y, label = attr(.x, 'label')),
      'character' = NominalVariable$new(name = .y, label = attr(.x, 'label')),
      'numeric' = NumericVariable$new(name = .y, label = attr(.x, 'label')),
      'integer' = NumericVariable$new(name = .y, label = attr(.x, 'label')),
      NULL
    )
  )

  keep <- which(purrr::map_lgl(vars, ~!is.null(.x)))

  DataDictionary$new(vars[keep])

}

#' Create a Numeric Variable
#'
#' Initializes a `NumericVariable` object, typically used to represent
#'   continuous or quantitative data.
#'
#' @param name Character.
#'   The name of the variable (required).
#' @param label Character.
#'   A short label for the variable (optional).
#' @param description Character.
#'   A longer description of the variable (optional).
#' @param group Character.
#'   The group or domain this variable belongs to (optional).
#' @param units Character.
#'   Variable units (e.g., "kg", "years") (optional).
#' @param divby_modeling Numeric.
#'   This constant indicates what the variable's values should be divided by
#'   before modeling. For example, if age is given in years with `divby_modeling`
#'   equal to 10, it means that age should be divided by 10 before it is
#'   used in a model, which means the interpretation of, e.g., a regression
#'   coefficient for age, will be expected change per 10 years (optional).
#'
#' @return A `NumericVariable` object.
#' @export
#'
#' @examples
#' age <- numeric_variable(
#'   name = "age",
#'   label = "Age of participant",
#'   units = "years",
#'   divby_modeling = 10
#' )
#' print(age)
#'
numeric_variable <- function(name,
                             label = NULL,
                             description = NULL,
                             group = NULL,
                             units = NULL,
                             divby_modeling = NULL){

  NumericVariable$new(name = name,
                      label = label,
                      description = description,
                      group = group,
                      units = units,
                      divby_modeling = divby_modeling)

}

#' Create a Nominal (Categorical) Variable
#'
#' Initializes a `NominalVariable` object, typically used to represent categorical data.
#'
#' @param name Character.
#'   The name of the variable (required).
#' @param label Character.
#'   A short label for the variable (optional).
#' @param description Character.
#'   A longer description of the variable (optional).
#' @param group Character.
#'   The group or domain this variable belongs to (optional).
#' @param category_levels Character vector.
#'   The set of unique category codes.
#' @param category_labels Character vector.
#'   Human-readable labels for the categories. If not provided, will default
#'   to `category_levels`.
#'
#' @return A `NominalVariable` object.
#' @export
#'
#' @examples
#' gender <- nominal_variable(
#'   name = "gender",
#'   label = "Gender",
#'   category_levels = c("M", "F"),
#'   category_labels = c("Male", "Female")
#' )
#' print(gender)
#'
nominal_variable <- function(name,
                             label = NULL,
                             description = NULL,
                             group = NULL,
                             category_levels = NULL,
                             category_labels = NULL){

  NominalVariable$new(name = name,
                      label = label,
                      description = description,
                      group = group,
                      category_levels = category_levels,
                      category_labels = category_labels)

}
