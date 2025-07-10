
data_test <- data.frame(
  number = 1:5,
  integer = 6L:10L,
  character = letters[1:5],
  factor = factor(letters[6:10]),
  date = as.POSIXct(Sys.time()+1:5)
)

dd <- as_data_dictionary(data_test) %>%
  set_labels(number = "A number",
             integer = "An integer",
             character = "A character",
             factor = "A factor") %>%
  set_units(number = 'quarks',
            integer = 'widgets')

recode_with_extras <- c("number", "integer", "extra_1", "extra_2")

test_that("unaccounted unique values are listed in warnings", code = {

  expect_warning(dd$recode(recode_with_extras,
                           extra_1 = "This extra is okay"),
                 regexp = "extra_2")

  expect_true("extra_2" %in% dd$recode(recode_with_extras,
                                       extra_1 = "This extra is okay",
                                       warn_unmatched = FALSE))

})


test_that("unaccounted unique values can be recoded on the fly", code = {

  expect_equal(
    dd$recode(recode_with_extras,
              extra_1 = "This extra is okay",
              extra_2 = "This is okay too"),
    c("A number", "An integer", "This extra is okay", "This is okay too")
  )

})

test_that("each recode feature works together at the same time", code = {

  expect_equal(
    dd$recode(recode_with_extras,
              units = 'descriptive',
              extra_1 = "This extra is okay",
              extra_2 = "This is okay too"),
    c("A number, quarks", "An integer, widgets",
      "This extra is okay", "This is okay too")
  )

})

test_that(
  "recode throws warning w/explanation for duplicated labels",
  code = {

    dd <- data_dictionary(
      nominal_variable('htn', 'hypertension', category_levels = c('no', 'yes')),
      nominal_variable('diab', 'diabetes', category_levels = c('no', 'yes')),
      nominal_variable('age', "age group", category_levels = c("first", "second")),
      nominal_variable("bmi", "bmu group", category_levels = c("first", "second"))
    ) %>%
      set_factor_labels(htn = c(no = "Nope", 'yes' = 'hypertension'),
                        diab = c(no = "Nope", 'yes' = 'diabetes'),
                        age = c("first" = "50-65"),
                        bmi = c("first" = "< 18"))

    expect_warning(dd$recode(c("no", "yes", "first")),
                   regexp = "multiple labels")

    expect_equal(dd$recode("no"), "Nope")

  }
)

test_that(
  "recode is unphased by duplicated binary levels w/out diverging labels",
  code = {

    dd <- data_dictionary(
      nominal_variable('htn', 'hypertension', category_levels = c('no', 'yes')),
      nominal_variable('diab', 'diabetes', category_levels = c('no', 'yes'))
    )

    expect_equal(dd$recode(c("no", "yes")), c("no", "yes"))

  }
)

