

age_years <- NumericVariable$new(
  name = "age_years",
  label = "Age",
  units = 'years',
  divby_modeling = 10
)

age_group <- NominalVariable$new(
  name = "age_group",
  label = "Age group",
  description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
  category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
  category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
)

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
  desc = "get and set", code = {

    expect_equal(age_years$get_category_labels(), 'none')

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
  desc = 'dictionary from base classes', code = {

    dd <- data_dictionary(age_years, age_group)

    expect_s3_class(dd, "DataDictionary")
    expect_s3_class(dd, "R6")
    expect_s3_class(dd$dictionary, "data.frame")
    expect_equal(dd$variables$age_years, age_years)
    expect_equal(dd$variables$age_group, age_group)


  }
)

data_test <- data.frame(
  number = 1:5,
  integer = 6L:10L,
  character = letters[1:5],
  factor = factor(letters[6:10]),
  date = as.POSIXct(Sys.time()+1:5)
)

test_that(
  desc = 'empty dictionary', code = {

    dd_empty <- data_test %>%
      as_data_dictionary()

    expect_true(all(dd_empty$dictionary$label == 'none'))
    expect_true(all(dd_empty$dictionary$description == 'none'))
    expect_true(all(dd_empty$dictionary$units == 'none'))
    expect_true(all(dd_empty$dictionary$divby_modeling == 'none'))
    expect_true(all(dd_empty$dictionary$category_levels == 'none'))
    expect_true(all(dd_empty$dictionary$category_labels == 'none'))

  }
)

test_that(
  desc = "Set dictionary values", code = {

    dd_set_label <- data_test %>%
      as_data_dictionary() %>%
      set_label(number = "A double value",
                integer = "An integer value")

    # access new values in dictionary

    expect_equal(as.character(dd_set_label$dictionary$label[1]),
                 "A double value")

    expect_equal(as.character(dd_set_label$dictionary$label[2]),
                 "An integer value")

    # access new values in variables

    expect_equal(dd_set_label$variables$number$get_label(),
                 "A double value")

    expect_equal(dd_set_label$variables$integer$get_label(),
                 "An integer value")

  }
)

test_that(
  desc = "incorrect inputs", code = {

    dd <- as_data_dictionary(data_test)

    expect_error(set_units(dd, factor = 'ohno'), regexp = 'nominal variable')
    expect_error(set_category_labels(dd, integer = 'ohno'), regexp = 'numeric variable')

  }
)
