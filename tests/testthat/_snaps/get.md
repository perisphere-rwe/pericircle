# get unknowns has expected output as request

    Code
      get_unknowns(iris, as_request = TRUE)
    Output
      A label to use for this variable in reports:
      
        - Sepal.Length = ?
        - Sepal.Width = ?
        - Petal.Length = ?
        - Petal.Width = ?
        - Species = ?
      
      Category labels for this variable (labels are shown in reports):
      
        - Species: setosa = ?;  versicolor = ?;  virginica = ?
      
      Variable units (e.g., age in years):
      
        - Sepal.Length = ?
        - Sepal.Width = ?
        - Petal.Length = ?
        - Petal.Width = ? 

# get unknowns has expected output as code

    Code
      get_unknowns(iris, as_code = TRUE)
    Output
      set_labels(Sepal.Length  = "",
                 Sepal.Width  = "",
                 Petal.Length  = "",
                 Petal.Width  = "",
                 Species  = "") %>% 
      set_factor_labels(Species = c(setosa = "",
                                    versicolor = "",
                                    virginica = "")) %>% 
      set_units(Sepal.Length  = "",
                Sepal.Width  = "",
                Petal.Length  = "",
                Petal.Width  = "") 

