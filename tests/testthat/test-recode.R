
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
             factor = "A factor")

recode_with_extras <- c("number", "integer", "extra_1", "extra_2")

test_that("unaccounted unique values are listed in errors", code = {

  expect_error(dd$recode(recode_with_extras,
                         extra_1 = "This extra is okay"),
               regexp = "extra_2")

})


test_that("unaccounted unique values can be recoded on the fly", code = {

  expect_equal(
    dd$recode(recode_with_extras,
              extra_1 = "This extra is okay",
              extra_2 = "This is okay too"),
    c("A number", "An integer", "This extra is okay", "This is okay too")
  )

})

