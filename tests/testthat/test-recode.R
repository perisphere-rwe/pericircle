
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

dd$recode(recode_with_extras)
