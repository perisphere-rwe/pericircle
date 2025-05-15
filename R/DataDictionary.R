
# DataVariable ----

DataVariable <- R6Class(

  "DataVariable",

  public = list(

    # relevant for all variables
    name = NULL,
    type = NULL,
    label = NULL,
    acronym = NULL,
    description = NULL,

    # only relevant for nominal variables but should be
    # set to a value of 'none' for numeric variables
    category_levels = NULL,
    category_labels = NULL,

    # only relevant for numeric variables but should be
    # set to a value of 'none' for nominal variables
    units = NULL,
    divby_modeling = NULL,

    # Checkers

    check_input = function(field, value){
      switch(field,
             "name"            = self$check_name(value),
             "type"            = self$check_type(value),
             "label"           = self$check_label(value),
             "acronym"         = self$check_acronym(value),
             "description"     = self$check_description(value),
             "units"           = self$check_units(value),
             "divby_modeling"  = self$check_divby_modeling(value),
             "category_levels" = self$check_category_levels(value),
             "category_labels" = self$check_category_labels(value))
    },

    check_name = function(value) {
      name <- value
      checkmate::assert_character(name, len = 1, any.missing = FALSE)
    },

    check_type = function(value) {
      type <- value
      checkmate::assert_character(type, len = 1, any.missing = FALSE)
      checkmate::assert_choice(type, choices = c("Data",
                                                 "Numeric",
                                                 "Nominal"))
    },

    check_label = function(value) {
      label <- value
      checkmate::assert_character(label,
                                  len = 1,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
    },

    check_acronym = function(value) {

      if(is.null(self$label)){
        stop("Acronyms cannot be set until label is set", call. = FALSE)
      }

      acronym <- value
      checkmate::assert_character(acronym,
                                  any.missing = FALSE,
                                  null.ok = TRUE,
                                  names = 'unique')

    },

    check_description = function(value) {
      description <- value
      checkmate::assert_character(description,
                                  len = 1,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
    },

    # Retrievers
    get_element = function(x){
      self[[x]]
    },

    fmt_element = function(x, collapse = ', '){

      if(is.null(self[[x]])) return("none")

      .values <- self$get_element(x)
      .names <- names(.values)

      if(!is.null(.names)){
        .values <- paste(.names, .values, sep = ' = ')
      }

      as.character(paste(.values, collapse = collapse))

    },

    set_element = function(field, value){

      self$check_input(field, value)

      current_value <- self$get_element(field)
      current_names <- names(current_value)

      if(!is.null(current_names)){

        # TODO: write a more specific error here
        new_value <- value
        stopifnot(all(names(new_value) %in% current_names))

        value <- current_value
        value[names(new_value)] <- new_value

      }

      self[[field]] <- value

    },

    # modify in place
    set_name = function(value) self$set_element("name", value),
    set_type = function(value) self$set_element("type", value),
    set_label = function(value) self$set_element("label", value),
    set_acronym = function(value) self$set_element("acronym", value),
    set_description = function(value) self$set_element("description", value),
    set_units = function(value) self$set_element("units", value),
    set_divby_modeling = function(value) self$set_element("divby_modeling", value),
    set_category_levels = function(value) self$set_element("category_levels", value),
    set_category_labels = function(value) self$set_element("category_labels", value),

    # return the value (can return NULL)
    get_name = function() self$get_element("name"),
    get_type = function() self$get_element("type"),

    get_label = function(use_acro = NULL){

      if(is.null(use_acro))
        return(self$get_element("label"))

      if(is.null(self$acronym) && !is.null(use_acro))
        stop("no acronyms for ", self$name, call. = FALSE)

      # if_else wouldn't work here - result needs to allow for length >1
      # ifelse doesn't work either - result needs to be named
      # => keep the clunky if() else statement
      pattern <- if(use_acro){
        purrr::set_names(names(self$acronym), self$acronym)
      }  else {
        self$acronym
      }

      stringr::str_replace_all(string = self$get_label(), pattern = pattern)

    },

    get_acronym = function() self$get_element("acronym"),
    get_description = function() self$get_element("description"),
    get_units = function() self$get_element("units"),
    get_divby_modeling = function() self$get_element("divby_modeling"),
    get_category_levels = function() self$get_element("category_levels"),
    get_category_labels = function(use_acro = NULL) self$get_element("category_labels"),

    # return a value, falling back to secondary options if needed
    fetch_name = function()
      self$get_name(),

    fetch_type = function()
      self$get_type(),

    fetch_label = function(use_acro=NULL)
      self$get_label(use_acro) %||% self$fetch_name(),

    fetch_acronym = function()
      self$get_acronym(),

    fetch_description = function()
      self$get_description() %||% self$fetch_label(),

    fetch_units = function()
      self$get_units(),

    fetch_divby_modeling = function()
      self$get_divby_modeling() %||% 1,

    fetch_category_levels = function()
      self$get_category_levels(),

    fetch_category_labels = function()
      self$get_category_labels() %||% self$fetch_category_levels(),

    # format value, can return 'none' when value is NULL
    fmt_name = function() self$fmt_element("name"),
    fmt_type = function() self$fmt_element("type"),
    fmt_label = function() self$fmt_element("label"),
    fmt_acronym = function() self$fmt_element("acronym"),
    fmt_description = function() self$fmt_element("description"),
    fmt_units = function() self$fmt_element("units"),
    fmt_divby_modeling = function() self$fmt_element("divby_modeling"),
    fmt_category_levels = function() self$fmt_element("category_levels"),
    fmt_category_labels = function() self$fmt_element("category_labels"),

    # Constructor
    initialize = function(name,
                          type = "Data",
                          label = NULL,
                          acronym = NULL,
                          description = NULL) {

      if (missing(name) || !is.character(name) || length(name) != 1) {
        stop("'name' must be a single character string and is required.",
             call. = FALSE)
      }


      self$check_name(name)
      self$check_type(type)
      self$check_label(label)
      self$check_description(description)

      self$name        <- name
      self$type        <- type
      self$label       <- label
      self$description <- description

      # can't check acronym until label is set
      self$check_acronym(acronym)
      self$acronym <- acronym

    },

    # Method to print object information
    print = function(...) {
      cat(self$fmt_type(), "Variable:\n")
      cat("  Name               :", self$fmt_name(),        "\n")
      cat("  Label              :", self$fmt_label(),       "\n")
      cat("  Acronym            :", self$fmt_acronym(),     "\n")
      cat("  Description        :", self$fmt_description(), "\n")
    }
  )

)

# NumericVariable ----

NumericVariable <- R6Class(

  "NumericVariable",

  inherit = DataVariable,  # Inherit from DataVariable

  public = list(

    check_category_levels = function(value) {
      if(!is.null(value)) stop(
        "category levels cannot be specified for a numeric variable",
        call. = FALSE
      )
    },

    check_category_labels = function(value) {
      if(!is.null(value)) stop(
        "category labels cannot be specified for a numeric variable",
        call. = FALSE
      )
    },

    check_acronym = function(value) {

      if(is.null(value)) return(value)

      # this ensures label isn't null
      super$check_acronym(value)

      label <- self$label

      acro_string_in_label <-
        stringr::str_detect(label, pattern = stringr::fixed(value))
      acro_abbrv_in_label <-
        stringr::str_detect(label, pattern = stringr::fixed(names(value)))

      if(!all(acro_string_in_label | acro_abbrv_in_label)){

        # check for case-sensitive mistake
        .label <- tolower(label)
        .value <- purrr::set_names(tolower(value), tolower(names(value)))

        .acro_string_in_label <-
          stringr::str_detect(.label, pattern = stringr::fixed(.value))
        .acro_abbrv_in_label <-
          stringr::str_detect(.label, pattern = stringr::fixed(names(.value)))

        .case_msg <- NULL

        if(all(.acro_string_in_label | .acro_abbrv_in_label)){
          .case_msg <-
            "\n  check capitalization - this is case-sensitive"
        }

        link <- paste(names(value), value, sep = " = ")

        stop(
          "could not match supplied acronym with label:\n",
          "  - label: ", label, "\n",
          "  - attempted match: ", paste(link, collapse = ", "), "\n",
          "  all acronym names or values must appear in label",
          .case_msg,
          call. = FALSE
        )


      }



    },

    check_units = function(value) {
      units <- value
      checkmate::assert_character(units,
                                  len = 1,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
    },

    check_divby_modeling = function(value) {
      divby_modeling <- value
      checkmate::assert_numeric(divby_modeling,
                                len = 1,
                                lower = 1,
                                any.missing = FALSE,
                                null.ok = TRUE)
    },

    get_label_and_unit = function(sep = ', ', use_acro=NULL){

      # present units if they are there
      if(!is.null(self$fetch_units)){
        return(paste(self$fetch_label(use_acro=use_acro),
                     self$fetch_units(),
                     sep = sep))
      }

      self$fetch_label(use_acro)

    },

    get_label_divby = function(use_acro=NULL){
      paste0(self$fetch_label(use_acro=use_acro), ", per ",
             self$fetch_divby_modeling(), " ",
             self$fetch_units() %||% "units")
    },

    # Constructor
    initialize = function(name,
                          label = NULL,
                          acronym = NULL,
                          description = NULL,
                          units = NULL,
                          divby_modeling = NULL) {

      # Call the parent class (DataVariable) initialize
      super$initialize(
        name = name,
        type = "Numeric",
        label = label,
        acronym = acronym,
        description = description
      )

      self$check_units(units)
      self$check_divby_modeling(divby_modeling)

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

NominalVariable <- R6Class(
  "NominalVariable",
  inherit = DataVariable,  # Inherit from DataVariable

  public = list(

    check_category_levels = function(value) {
      category_levels <- value
      checkmate::assert_character(category_levels,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
    },

    check_category_labels = function(value) {
      category_labels <- value
      checkmate::assert_character(category_labels,
                                  any.missing = FALSE,
                                  null.ok = TRUE)
    },

    check_acronym = function(value){

      if(is.null(value)) return(value)

      # this ensures label isn't null
      super$check_acronym(value)

      label <- self$label
      category_labels <- self$fetch_category_labels()

      acro_string_in_label <-
        stringr::str_detect(label,
                            pattern = stringr::fixed(value))

      acro_string_in_category_labels <-
        stringr::str_detect(category_labels,
                            pattern = stringr::fixed(value))

      acro_abbrv_in_label <-
        stringr::str_detect(label,
                            pattern = stringr::fixed(names(value)))

      acro_abbrv_in_category_labels <-
        stringr::str_detect(category_labels,
                            pattern = stringr::fixed(names(value)))

      if(!all(acro_string_in_label |
              acro_abbrv_in_label |
              acro_string_in_category_labels |
              acro_abbrv_in_category_labels)){
        stop(
          "could not match supplied acronym with label or categories:\n",
          "  - acronym names: ", paste(names(value), collapse = ", "), "\n",
          "  - acronym values: ", paste(value, collapse = ", "), "\n",
          "  - label: ", label, "\n",
          "  - categories: ", paste(category_labels, collapse = ', '),
          "  all acronym names or values must appear in label or categories"
        )
      }

    },

    check_units = function(value) {
      if(!is.null(value)) stop(
        "units cannot be specified for a nominal variable",
        call. = FALSE
      )
    },

    check_divby_modeling = function(value) {
      if(!is.null(value)) stop(
        "divby_modeling cannot be specified for a nominal variable",
        call. = FALSE
      )
    },

    get_label_and_unit = function(sep = ', ', use_acro=NULL){
      self$fetch_label(use_acro=use_acro)
    },

    get_label_divby = function(use_acro=NULL){
      self$fetch_label(use_acro=use_acro)
    },

    get_category_labels = function(use_acro = NULL){

      if(is.null(use_acro))
        return(self$get_element("category_labels"))

      if(is.null(self$acronym) && !is.null(use_acro))
        stop("no acronyms for ", self$name, call. = FALSE)

      # if_else wouldn't work here - result needs to allow for length >1
      # ifelse doesn't work either - result needs to be named
      # => keep the clunky if() else statement
      pattern <- if(use_acro){
        purrr::set_names(names(self$acronym), self$acronym)
      } else {
        self$acronym
      }

      stringr::str_replace_all(string = self$get_category_labels(),
                               pattern = pattern)

    },

    # Constructor
    initialize = function(name,
                          label = NULL,
                          acronym = NULL,
                          description = NULL,
                          category_levels = NULL,
                          category_labels = NULL) {

      # Call parent class initialize
      super$initialize(
        name = name,
        type = "Nominal",
        label = label,
        acronym = acronym,
        description = description
      )

      self$check_category_levels(category_levels)
      self$check_category_labels(category_labels)

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

DataDictionary <- R6Class(

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

    get_label = function(name, units = 'none', use_acro = NULL){

      switch(
        units,
        'none'        = self$variables[[name]]$get_label(use_acro=use_acro),
        'descriptive' = self$variables[[name]]$get_label_and_unit(use_acro=use_acro),
        'model'       = self$variables[[name]]$get_label_divby(use_acro=use_acro)
      )

    },

    get_variable_names = function(){
      names(self$variables)
    },

    get_variable_recoder = function(name = NULL,
                                    units = 'none',
                                    use_acro = NULL,
                                    quiet = FALSE){

      checkmate::assert_character(name, null.ok = TRUE)

      .name <- name %||% names(self$variables)

      for(i in seq_along(.name)){
        checkmate::assert_choice(.name[i],
                                 .var.name = .name[i],
                                 choices = names(self$variables))
      }

      output <- purrr::map(
        .x = purrr::set_names(.name),
        .f = ~ self$get_label(.x, units = units, use_acro = use_acro)
      ) %>%
        purrr::compact()

      unlabeled <- setdiff(.name, names(output))

      if(!is_empty(unlabeled)){

        if(!quiet){
          warning("Incomplete recode information for {",
                  paste(unlabeled, collapse = ", "),
                  "} : labels are missing.",
                  call. = FALSE)
        }

      }

      output

    },

    get_level_recoder = function(name = NULL, use_acro = NULL, quiet = FALSE){

      checkmate::assert_character(name, null.ok = TRUE)

      choices <- self$variables %>%
        purrr::map_lgl(~.x$type == "Nominal") %>%
        which() %>%
        names()

      name <- name %||% choices

      output <- character(length = 0L)

      for(i in seq_along(name)){

        checkmate::assert_choice(name[i], choices = choices)

        .labs <- self$variables[[ name[i] ]]$category_labels
        .lvls <- self$variables[[ name[i] ]]$category_levels

        if(!is.null(use_acro)){
          browser()
        }

        if(is.null(.labs)){

          if(!quiet){
            warning("Recode information for variable {", name[i],
                    "} is incomplete: labels are missing",
                    call. = FALSE)
          }

          next

        }

        output %<>% append(
          values = purrr::set_names(
            x = self$variables[[ name[i] ]]$category_labels,
            nm = self$variables[[ name[i] ]]$category_levels
          )
        )

      }

      output

    },

    recode = function(x, ..., units = 'none', use_acro = NULL){

      x_uni <- unique(stats::na.omit(x))

      variable_recoder <- self$get_variable_recoder(quiet = TRUE, units = units)
      level_recoder <- self$get_level_recoder(quiet = TRUE, use_acro = use_acro)

      if(!is_empty(list(...))){
        variable_recoder %<>% c(list(...))
        level_recoder %<>% c(list(...))
      }

      x_is_variable_names <- all(x_uni %in% names(variable_recoder))
      x_is_variable_levels <- all(x_uni %in% names(level_recoder))

      if(x_is_variable_levels && x_is_variable_names){
        stop("unique values of x were found to be present in ",
             "both the variable labels and the category labels ",
             "for the given dictionary. It is not clear which ",
             "of these should be used to recode values of x.",
             call. = FALSE)
      }

      if(x_is_variable_levels){

        if(any(duplicated(level_recoder))){
          stop("there are duplicated levels in the data dictionary. ",
               "This occurs when one or more variables have identical ",
               "labels, and it prevents `recode()` from identifying ",
               "which label to attach to the given x values.",
               call. = FALSE)
        }

        return(dplyr::recode(x, !!!level_recoder))

      }

      if(x_is_variable_names){
        return(dplyr::recode(x, !!!variable_recoder))
      }

      leftovers <- setdiff(x_uni, c(names(variable_recoder),
                                    names(level_recoder)))

      stop("Unique values in x could not be matched with variable labels ",
           "or variable level labels in the dictionary. The x values that ",
           "could not be matched are: ", paste(leftovers, collapse = ", "),
           call. = FALSE)

    },

    # Function to create tibble summary of variables
    create_dictionary = function(vars) {

      tibble::tibble(
        name            = purrr::map_chr(vars, ~ .x$fmt_name()),
        label           = purrr::map_chr(vars, ~ .x$fmt_label()),
        description     = purrr::map_chr(vars, ~ .x$fmt_description()),
        acronym         = purrr::map_chr(vars, ~ .x$fmt_acronym()),
        units           = purrr::map_chr(vars, ~ .x$fmt_units()),
        divby_modeling  = purrr::map_chr(vars, ~ .x$fmt_divby_modeling()),
        category_levels = purrr::map_chr(vars, ~ .x$fmt_category_levels()),
        category_labels = purrr::map_chr(vars, ~ .x$fmt_category_labels())
      )

    },

    modify_dictionary = function(key, field){

      chk_inputs <-
        checkmate::check_character(names(key),
                                   any.missing = FALSE,
                                   unique = TRUE,
                                   null.ok = FALSE)

      throw_check(chk_inputs)

      for(i in seq_along(names(key))){

        chk_i <- checkmate::check_choice(
          x = names(key)[i],
          choices = self$dictionary$name
        )

        throw_check(chk_i, pre_text = "Input name not recognized. ")

      }

      for(i in names(key)){
        self$variables[[i]]$set_element(field = field, value = key[[i]])
      }

      # Extract data for the tibble
      self$dictionary <- self$create_dictionary(self$variables)

    },

    # Print method
    print = function(...) {

      cat("Data Dictionary:\n")
      print(self$dictionary, n = Inf)

    }
  )

)

#' Create Data Dictionary from Variable Definitions
#'
#' Initializes a `DataDictionary` object from a set of variables.
#' Variables should be made using [numeric_variable()] or [nominal_variable()]
#' and passed directly as arguments.
#'
#' @param ... One or more objects inheriting from `r roxy_describe_numeric()`
#'   or `r roxy_describe_nominal()`.
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
#' dd <- data_dictionary(age_years, gender)
#' print(dd)
#'
data_dictionary <- function(...){

  DataDictionary$new(list(...))

}


#' Create Data Dictionary from Existing Data
#'
#' @param x a data frame
#'
#' @return `r roxy_describe_dd()`
#'
#' @export
#'
#' @examples
#'
#' attr(iris$Species, 'label') <- "Flower species"
#'
#' as_data_dictionary(iris)
#'
as_data_dictionary <- function(x){

  stopifnot(is.data.frame(x))

  vars <- purrr::map2(
    .x = x,
    .y = names(x),
    .f = ~ switch(
      class(.x)[1],
      'factor' = NominalVariable$new(
        name = .y,
        label = attr(.x, 'label'),
        category_levels = levels(.x)
      ),
      'character' = NominalVariable$new(
        name = .y,
        label = attr(.x, 'label'),
        category_levels = unique(stats::na.omit(.x))
      ),
      'numeric' = NumericVariable$new(
        name = .y,
        label = attr(.x, 'label')
      ),
      'integer' = NumericVariable$new(
        name = .y,
        label = attr(.x, 'label')
      ),
      NULL
    )
  )

  keep <- which(purrr::map_lgl(vars, ~!is.null(.x)))

  DataDictionary$new(vars[keep])

}

# Variable functions ----

#' Create a Numeric Variable
#'
#' Initializes a `NumericVariable` object, typically used to represent
#'   continuous or quantitative data.
#'
#' @param name Character.
#'   The name of the variable (required).
#' @param label Character.
#'   A short label for the variable (optional).
#' @param acronym Character.
#'   An acronym for the label (e.g., "BP = blood pressure"; optional).
#' @param description Character.
#'   A longer description of the variable (optional).
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
#'
#' age <- numeric_variable(
#'   name = "age",
#'   label = "Age of participant",
#'   units = "years",
#'   divby_modeling = 10
#' )
#'
#' age
#'
#'
#' age_group <- nominal_variable(
#'   name = "age_group",
#'   label = "Age group",
#'   description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
#'   category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
#'   category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
#' )
#'
#' age_group

numeric_variable <- function(name,
                             label = NULL,
                             acronym = NULL,
                             description = NULL,
                             units = NULL,
                             divby_modeling = NULL){

  NumericVariable$new(name = name,
                      label = label,
                      acronym = acronym,
                      description = description,
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
#' @param acronym Character.
#'   An acronym for the label (e.g., "BP = blood pressure"; optional).
#' @param description Character.
#'   A longer description of the variable (optional).
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
                             acronym = NULL,
                             description = NULL,
                             category_levels = NULL,
                             category_labels = NULL){

  NominalVariable$new(name = name,
                      label = label,
                      acronym = acronym,
                      description = description,
                      category_levels = category_levels,
                      category_labels = category_labels)

}
