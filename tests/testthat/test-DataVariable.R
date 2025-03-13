

# Example usage:
age_years <- numeric_variable(
  name = "age",
  label = "Age of participant",
  description = "The age of the participant at date_of_measurement",
  group = "Demographics",
  dependencies = c("date_of_birth", "date_of_measurement"),
  units_descriptive = "years",
  divby_modeling = 10
)

age_group <- nominal_variable(
  name = "age_group",
  label = "Age group",
  description = "Ages of 0 to < 50, 50 to < 60, and \u2265 60 years",
  group = "Demographics",
  dependencies = "age_years",
  category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
  category_labels = c("0 to < 50", "50 to < 60", "\u2265 60")
)

# Print the variable
age_years
age_group

dd <- data_dictionary(age_years, age_group)


