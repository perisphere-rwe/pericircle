
# testing objects ----

age_years <- numeric_variable(
  name = "age_years",
  label = "Age",
  units = 'years',
  divby_modeling = 10
)

age_group <- nominal_variable(
  name = "age_group",
  label = "Age group",
  description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
  category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
  category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
)

date_recorded <- date_variable(
  name = "date_recorded",
  label = "Date of observation",
  description = "The calendar date when the observation was recorded"
)

data_test <- data.frame(
  number = 1:5,
  integer = 6L:10L,
  character = letters[1:5],
  factor = factor(letters[6:10]),
  date = as.POSIXct(Sys.time()+1:5)
)

test_that(
  desc = "construction works from list or ..., not both",
  code = {

    dd_1 <-
      data_dictionary(variable_list = list(age_years, age_group))

    dd_2 <-
      data_dictionary(age_years, age_group)

    expect_equal(dd_1, dd_2)

  }
)

dd <- as_data_dictionary(data_test)


# required inputs ----

test_that(

  desc = "name required", code = {

    expect_error(numeric_variable(), 'name')

  }

)

# inheritance ----

test_that(

  desc = "Class structure", code = {

    expect_s3_class(age_years, "NumericVariable")
    expect_s3_class(age_years, "DataVariable")
    expect_s3_class(age_years, "R6")

    expect_s3_class(age_group, "NominalVariable")
    expect_s3_class(age_group, "DataVariable")
    expect_s3_class(age_group, "R6")

    expect_s3_class(date_recorded, "DateVariable")
    expect_s3_class(date_recorded, "DataVariable")
    expect_s3_class(date_recorded, "R6")

  }

)

test_that(

  desc = 'dictionary from base classes', code = {

    dd_from_vars <- data_dictionary(age_years, age_group, date_recorded)

    expect_s3_class(dd_from_vars, "DataDictionary")
    expect_s3_class(dd_from_vars, "R6")
    expect_s3_class(dd_from_vars$dictionary, "data.frame")
    expect_equal(dd_from_vars$variables$age_years, age_years)
    expect_equal(dd_from_vars$variables$age_group, age_group)
    expect_equal(dd_from_vars$variables$date_recorded, date_recorded)

  }

)

# dictionary from data ----

test_that(

  desc = 'empty dictionary', code = {

    expect_true(all(dd$dictionary$label == 'none'))
    expect_true(all(dd$dictionary$description == 'none'))
    expect_true(all(dd$dictionary$units == 'none'))
    expect_true(all(dd$dictionary$divby_modeling == 'none'))

  }

)

test_that(

  desc = "dictionary handles date columns", code = {

  expect_true("date" %in% dd$get_variable_names())
  expect_s3_class(dd$variables$date, "DateVariable")

  }

)

# get and set ----

test_that(

  desc = "get and set", code = {

    expect_equal(age_years$get_category_labels(), NULL)
    expect_equal(age_years$fmt_category_labels(), "none")

    expect_equal(date_recorded$get_units(), NULL)
    expect_equal(date_recorded$get_category_levels(), NULL)
    expect_equal(date_recorded$fmt_divby_modeling(), 'none')


    expect_equal(age_group$get_category_labels(),
                 c("0 to < 50", "50 to < 60", "\u2265 60"))

    # not allowed
    expect_error(age_years$set_category_levels(value = 'no ty'))
    expect_error(age_group$set_units(value = 'no ty'))
    expect_error(date_recorded$set_units("days"))

    # okay
    age_years$set_units('days')
    expect_equal(age_years$get_units(), 'days')

    age_group$set_label('Age groups')
    expect_equal(age_group$get_label(), 'Age groups')

    date_recorded$set_label("Date collected")
    expect_equal(date_recorded$get_label(), "Date collected")

  }

)


test_that(

  desc = "Set dictionary values", code = {

    dd_set_label <- data_test %>%
      as_data_dictionary() %>%
      set_labels(number = "A double value",
                 integer = "An integer value") %>%
      set_descriptions(factor = "A factor variable with one level") %>%
      set_divby_modeling(number = 10)

    # access new values in dictionary

    expect_equal(dd_set_label$dictionary$label[1],
                 c(number = "A double value"))

    expect_equal(dd_set_label$variables$integer$get_label(),
                 "An integer value")

    expect_equal(dd_set_label$variables$factor$get_description(),
                 "A factor variable with one level")

    expect_equal(dd_set_label$variables$number$get_divby_modeling(), 10)

    # access new values in variables

    expect_equal(dd_set_label$variables$number$get_label(),
                 "A double value")

    expect_equal(dd_set_label$variables$integer$get_label(),
                 "An integer value")

  }

)

# errors ----

test_that(

  desc = "incorrect inputs", code = {


    expect_error(set_units(dd, factor = 'ohno'),
                 regexp = 'nominal variable')

    expect_error(set_divby_modeling(dd, factor = 'ohno'),
                 regexp = 'nominal variable')

    expect_error(set_factor_labels(dd, integer = c('ohno'='ohnooo')),
                 regexp = 'only nominal')

    expect_error(set_factor_order(dd, number = 'ohno'),
                 regexp = 'only nominal')


    expect_error(DataDictionary$new(vars = list()), 'At least one variable')

    expect_error(DataDictionary$new(vars = list(1)), 'inherit from')


  }

)

# snapshots ----

test_that(

  desc = "Printed output", code = {

    expect_snapshot(

      # example code for numeric variable
      numeric_variable(
        name = "age",
        label = "Age of participant",
        units = "years",
        divby_modeling = 10
      )

    )

    expect_snapshot(

      # example code for nominal variable
      nominal_variable(
        name = "age_group",
        label = "Age group",
        description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
        category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
        category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
      )

    )

    expect_snapshot(

      # example code for set functions
      as_data_dictionary(data.frame(a = 1, b = "cat")) %>%
        set_labels(a = "example", b = "categorical example") %>%
        set_units(a = "years") %>%
        set_descriptions(a = "A variable used for examples") %>%
        set_factor_labels(b = c(cat = "A small lion"))

    )

  }

)

