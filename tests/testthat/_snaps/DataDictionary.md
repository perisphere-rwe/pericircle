# Printed output

    Code
      numeric_variable(name = "age", label = "Age of participant", units = "years",
        divby_modeling = 10)
    Output
      Numeric Variable:
        Name               : age 
        Label              : Age of participant 
        Acronym            : none 
        Description        : none 
        Units              : years 
        Modeling Divisor   : 10 

---

    Code
      nominal_variable(name = "age_group", label = "Age group", description = "Ages of 0 to < 50, 50 to < 60, and ≥ 60 years",
        category_levels = c("age_lt_50", "age_gteq_50_lt_60", "age_gteq_60"),
        category_labels = c("0 to < 50", "50 to < 60", "≥ 60"))
    Output
      Nominal Variable:
        Name               : age_group 
        Label              : Age group 
        Acronym            : none 
        Description        : Ages of 0 to < 50, 50 to < 60, and ≥ 60 years 
        Category Levels    : age_lt_50, age_gteq_50_lt_60, age_gteq_60 
        Category Labels    : 0 to < 50, 50 to < 60, ≥ 60 

---

    Code
      as_data_dictionary(data.frame(a = 1, b = "cat")) %>% set_labels(a = "example",
        b = "categorical example") %>% set_units(a = "years") %>% set_descriptions(a = "A variable used for examples") %>%
        set_category_labels(b = "A small lion")
    Output
      Data Dictionary:
      # A tibble: 2 x 8
        name  label           description acronym units divby_modeling category_levels
        <chr> <chr>           <chr>       <chr>   <chr> <chr>          <chr>          
      1 a     example         A variable~ none    years none           none           
      2 b     categorical ex~ none        none    none  none           cat            
      # i 1 more variable: category_labels <chr>

