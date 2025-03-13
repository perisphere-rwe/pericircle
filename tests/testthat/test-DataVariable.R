

age_years <- NumericVariable$new(
  name = "age",
  label = "Age",
  description = "Age measured at baseline exam",
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

# Print the variable
age_years
age_group

dd <- data_dictionary(age_years, age_group)

dd$modify_dictionary(age = 'days', field = 'units')

data_test <- data.frame(
  number = 1,
  integer = 1L,
  character = 'a',
  factor = factor("f"),
  date = as.POSIXct(Sys.time())
)

as_data_dictionary(data_test)
