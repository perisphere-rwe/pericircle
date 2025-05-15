
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

sbp_mmhg <- numeric_variable(
  name = "sbp",
  label = "Systolic blood pressure",
  acronym = c(BP = "blood pressure"),
  units = 'mm Hg'
)

bp_group <- nominal_variable(
  name = "bp_group",
  label = "BP group",
  acronym = c(SBP = "systolic blood pressure",
              DBP = "diastolic blood pressure",
              BP = "blood pressure"),
  description = "Test acronym usage in nominal variable",
  category_levels = c("normotensive", "bp_130_80", "bp_140_90"),
  category_labels = c("SBP/DBP < 130/80",
                      "SBP/DBP >= 130/80",
                      "SBP/DBP >= 140/90")
)

data_test <- data.frame(
  number = 1:5,
  integer = 6L:10L,
  character = letters[1:5],
  factor = factor(letters[6:10]),
  date = as.POSIXct(Sys.time()+1:5)
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

  }

)

test_that(

  desc = 'dictionary from base classes', code = {

    dd_from_vars <- data_dictionary(age_years, age_group,
                                    sbp_mmhg, bp_group)

    expect_s3_class(dd_from_vars, "DataDictionary")
    expect_s3_class(dd_from_vars, "R6")
    expect_s3_class(dd_from_vars$dictionary, "data.frame")
    expect_equal(dd_from_vars$variables$age_years, age_years)
    expect_equal(dd_from_vars$variables$age_group, age_group)


  }

)

# acronyms ----

test_that(

  desc = "acronym substitution", code = {

    expect_equal(sbp_mmhg$get_label(use_acro = TRUE),
                 "Systolic BP")

    expect_equal(sbp_mmhg$get_label(use_acro = FALSE),
                 "Systolic blood pressure")

    expect_equal(bp_group$get_label(use_acro = TRUE),
                 "BP group")

    expect_equal(bp_group$get_category_labels(use_acro = TRUE),
                 c("SBP/DBP < 130/80",
                   "SBP/DBP >= 130/80",
                   "SBP/DBP >= 140/90"))

    expect_equal(
      bp_group$get_category_labels(use_acro = FALSE),
      c("systolic blood pressure/diastolic blood pressure < 130/80",
        "systolic blood pressure/diastolic blood pressure >= 130/80",
        "systolic blood pressure/diastolic blood pressure >= 140/90"))

  }

)

test_that(

  desc = "acronym case sensitivity", code = {

    expect_error(
      sbp_mmhg$set_acronym(value = c("BP" = "Blood pressure")),
      regexp = 'check capitalization'
    )

    bp_group$set_acronym(c("SBP" = "Systolic blood pressure"))
    bp_group$get_category_labels(use_acro = FALSE)

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

# get and set ----

test_that(

  desc = "get and set", code = {

    expect_equal(age_years$get_category_labels(), NULL)
    expect_equal(age_years$fmt_category_labels(), "none")

    expect_equal(age_group$get_category_labels(),
                 c("0 to < 50", "50 to < 60", "\u2265 60"))

    # not allowed
    expect_error(age_years$set_category_levels(value = 'no ty'))
    expect_error(age_group$set_units(value = 'no ty'))

    # okay
    age_years$set_units('days')
    expect_equal(age_years$get_units(), 'days')

    age_group$set_label('Age groups')
    expect_equal(age_group$get_label(), 'Age groups')

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

    expect_error(set_category_labels(dd, integer = 'ohno'),
                 regexp = 'numeric variable')

    expect_error(set_category_levels(dd, number = 'ohno'),
                 regexp = 'numeric variable')


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
        set_category_labels(b = "A small lion")

    )

  }

)

