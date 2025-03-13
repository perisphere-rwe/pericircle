
DataVariable <- R6::R6Class(
  "DataVariable",

  public = list(
    # Public fields
    name = NULL,
    label = NULL,
    description = NULL,
    group = NULL,
    dependencies = NULL,


    # Retrievers
    get_element = function(name){
      if(is.null(self[[name]])) return(NA)
      self[[name]]
    },

    get_name = function() self$get_element("name"),
    get_label = function() self$get_element("label"),
    get_description = function() self$get_element("description"),
    get_group = function() self$get_element("group"),
    get_dependencies = function() self$get_element("dependencies"),
    get_units_descriptive = function() self$get_element("units_descriptive"),
    get_divby_modeling = function() self$get_element("divby_modeling"),
    get_category_levels = function() self$get_element("category_levels"),
    get_category_labels = function() self$get_element("category_labels"),

    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          group = NULL,
                          dependencies = NULL) {
      if (missing(name) || !is.character(name) || length(name) != 1) {
        stop("'name' must be a single character string and is required.")
      }
      self$name <- name
      self$label <- label
      self$description <- description
      self$group <- group
      self$dependencies <- dependencies
    },

    # Method to print object information
    print = function(...) {
      cat("Variable:\n")
      cat("  Name               :",
          self$name, "\n")
      cat("  Label              :",
          ifelse(is.null(self$label), "None", self$label), "\n")
      cat("  Description        :",
          ifelse(is.null(self$description), "None", self$description), "\n")
      cat("  Group              :",
          ifelse(is.null(self$group), "None", self$group), "\n")
      cat("  Dependencies       :",
          ifelse(is.null(self$dependencies),
                 "None",
                 paste(self$dependencies, collapse = ", ")), "\n")
    }
  )
)

NumericVariable <- R6::R6Class(
  "NumericVariable",
  inherit = DataVariable,  # Inherit from DataVariable

  public = list(
    # Additional fields specific to NumericVariable
    units_descriptive = NULL,
    divby_modeling = NULL,

    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          group = NULL,
                          dependencies = NULL,
                          units_descriptive = NULL,
                          divby_modeling = NULL) {

      # Call the parent class (DataVariable) initialize
      super$initialize(
        name = name,
        label = label,
        description = description,
        group = group,
        dependencies = dependencies
      )

      # Set additional fields
      self$units_descriptive <- units_descriptive
      self$divby_modeling <- divby_modeling
    },

    # Overriding print method to include unit information
    print = function(...) {
      super$print(...)  # Call the parent class print
      cat("  Units (Descriptive):",
          ifelse(is.null(self$units_descriptive), "None", self$units_descriptive), "\n")
      cat("  Units (Modeling)   :",
          ifelse(is.null(self$divby_modeling) | is.null(self$units_descriptive),
                 "None",
                 paste("per", self$divby_modeling, self$units_descriptive)), "\n")
    }
  )
)


NominalVariable <- R6::R6Class(
  "NominalVariable",
  inherit = DataVariable,  # Inherit from DataVariable

  public = list(
    # Additional fields specific to NominalVariable
    category_levels = NULL,
    category_labels = NULL,

    # Constructor
    initialize = function(name,
                          label = NULL,
                          description = NULL,
                          group = NULL,
                          dependencies = NULL,
                          category_levels = NULL,
                          category_labels = NULL) {

      # Call parent class initialize
      super$initialize(
        name = name,
        label = label,
        description = description,
        group = group,
        dependencies = dependencies
      )

      # Ensure category_levels are provided and are unique
      if (!is.null(category_levels)) {
        if (any(duplicated(category_levels))) {
          stop("'category_levels' must be unique. Duplicated values found.")
        }
        self$category_levels <- category_levels
      }

      # If category_labels are not provided, default them to category_levels
      if (is.null(category_labels) && !is.null(category_levels)) {
        self$category_labels <- as.character(category_levels)
      } else if (!is.null(category_labels)) {
        # If both provided, ensure lengths match
        if (length(category_labels) != length(category_levels)) {
          stop("'category_labels' must be the same length as 'category_levels'.")
        }
        self$category_labels <- category_labels
      }

      # Set additional fields
      self$category_levels <- category_levels
      self$category_labels <- category_labels

      # Validate that levels and labels (if both provided) are of same length
      if (!is.null(self$category_levels) && !is.null(self$category_labels)) {
        if (length(self$category_levels) != length(self$category_labels)) {
          stop("'category_levels' and 'category_labels' must be of the same length.")
        }
      }
    },

    # Overriding print method to include category information
    print = function(...) {
      super$print(...)  # Call the parent class print

      # Print category levels and labels nicely
      if (!is.null(self$category_levels)) {
        cat("  Category Levels    :", paste(self$category_levels, collapse = ", "), "\n")
      } else {
        cat("  Category Levels    : None\n")
      }

      if (!is.null(self$category_labels)) {
        cat("  Category Labels    :", paste(self$category_labels, collapse = ", "), "\n")
      } else {
        cat("  Category Labels    : None\n")
      }
    }
  )
)

DataDictionary <- R6::R6Class(
  "DataDictionary",

  public = list(
    # Store variables
    variables = NULL,
    # Store tibble summary
    dictionary = NULL,

    # Constructor
    initialize = function(...) {
      # Capture all input objects
      vars <- list(...)

      # Validate that all are instances of DataVariable or its children
      if (length(vars) == 0) {
        stop("At least one variable must be provided to create a DataDictionary.")
      }
      if (!all(purrr::map_lgl(vars, ~ inherits(.x, "DataVariable")))) {
        stop("All inputs must inherit from 'DataVariable' (e.g., NumericVariable, NominalVariable).")
      }

      # Store variables list
      self$variables <- vars

      # Extract data for the tibble
      self$dictionary <- self$create_dictionary_tibble(vars)
    },

    # Function to create tibble summary of variables
    create_dictionary_tibble = function(vars) {

      tibble::tibble(

        name = purrr::map_chr(vars, ~ .x$get_name()),

        label = purrr::map_chr(vars, ~ .x$get_label()),

        description = purrr::map_chr(vars, ~ .x$get_description()),

        group = purrr::map_chr(vars, ~ .x$get_group()),

        dependencies = purrr::map_chr(vars, ~ {
          stringr::str_c(.x$get_dependencies(), collapse = ', ')
        }),

        units_descriptive = purrr::map_chr(vars, ~ .x$get_units_descriptive()),

        divby_modeling = purrr::map_dbl(vars, ~ .x$get_divby_modeling()),

        category_levels = purrr::map_chr(vars, ~ {
          stringr::str_c(.x$get_category_levels(), collapse = ', ')
        }),

        category_labels = purrr::map_chr(vars, ~ {
          stringr::str_c(.x$get_category_levels(), collapse = ', ')
        })
      )
    },

    # Print method
    print = function(...) {
      cat("Data Dictionary:\n")
      print(self$dictionary, n = Inf)
    }
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


#' Create a Data Dictionary
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
#'   units_descriptive = "years"
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
  DataDictionary$new(...)
}

#' Create a Numeric Variable
#'
#' Initializes a `NumericVariable` object, typically used to represent continuous or quantitative data.
#'
#' @param name Character.
#'   The name of the variable (required).
#' @param label Character.
#'   A short label for the variable (optional).
#' @param description Character.
#'   A longer description of the variable (optional).
#' @param group Character.
#'   The group or domain this variable belongs to (optional).
#' @param dependencies Character vector.
#'   Names of other variables this variable depends on (optional).
#' @param units_descriptive Character.
#'   Human-readable units (e.g., "kg", "years") (optional).
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
#'   units_descriptive = "years",
#'   divby_modeling = 10
#' )
#' print(age)
#'
numeric_variable <- function(name,
                             label = NULL,
                             description = NULL,
                             group = NULL,
                             dependencies = NULL,
                             units_descriptive = NULL,
                             divby_modeling = NULL){

  NumericVariable$new(name = name,
                      label = label,
                      description = description,
                      group = group,
                      dependencies = dependencies,
                      units_descriptive = units_descriptive,
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
#' @param dependencies Character vector.
#'   Names of other variables this variable depends on (optional).
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
                             dependencies = NULL,
                             category_levels = NULL,
                             category_labels = NULL){

  NominalVariable$new(name = name,
                      label = label,
                      description = description,
                      group = group,
                      dependencies = dependencies,
                      category_levels = category_levels,
                      category_labels = category_labels)

}
