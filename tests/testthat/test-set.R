
data_test <- data.frame(
  number = 1:5,
  integer = 6L:10L,
  character = letters[1:5],
  factor = factor(letters[6:10]),
  date = as.POSIXct(Sys.time()+1:5)
)

dd <- as_data_dictionary(data_test)

test_that(
  desc = "order is preserved when you set factor labels out of order",
  code = {

    dd_wonk <- set_factor_labels(dd, character = c("e" = "E", "b" = "B"))

    expect_equal(dd_wonk$variables$character$category_labels,
                 c("a", "B", "c", "d", "E"))

  }
)

test_that(
  desc = "tell me what variable is out of place",
  code = {

    expect_error(set_labels(dd, not_in_there = "tell me"),
                 regexp = "Inputs must match")

  }
)

