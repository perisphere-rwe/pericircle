
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pericircle

<!-- badges: start -->
<!-- badges: end -->

A slice of Perisphere, `pericircle` aims to allow straightforward
application of our principals for project organization. These principals
are:

1.  Readable code.
2.  Minimal redundancy.
3.  Consistent presentation.

## Installation

You can install the development version of `pericircle` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("perisphere-rwe/pericircle")
```

## Data dictionaries

Dictionaries help organize pertinent information about analytic
variables, putting information about each variable into tables and
figures using consistent labels and incorporating additional information
when appropriate.

There are two ways to initialize a dictionary. You can build them using
`NumericVariable` and `NominalVariable` objects (not shown here, see
vignette), but this can be tedious if you have a lot of variables. For
most cases, you’ll want to use the `as_data_dictionary()` to create a
starter dictionary from a given dataset.

``` r

library(pericircle)
library(tidyverse)
library(palmerpenguins)

data_peng <- penguins %>% 
  select(species, sex, body_mass_g, bill_length_mm, bill_depth_mm)

dd_peng <- as_data_dictionary(data_peng)

dd_peng
#> Data Dictionary:
#> # A tibble: 5 × 7
#>   name    label description units divby_modeling category_levels category_labels
#>   <chr>   <chr> <chr>       <chr> <chr>          <chr>           <chr>          
#> 1 species none  none        none  none           Adelie, Chinst… none           
#> 2 sex     none  none        none  none           female, male    none           
#> 3 body_m… none  none        none  none           none            none           
#> 4 bill_l… none  none        none  none           none            none           
#> 5 bill_d… none  none        none  none           none            none
```

### Tell me what I don’t know

Our dictionary is initialized, but it doesn’t contain complete
information yet. Where should we start? Use the function
`get_unknowns()` to clarify what relevant information is missing. This
function returns a `tibble` by default, but if you set
`as_request = TRUE`, it provides a bullet point list that is easier to
read. If you work with subject matter experts, you can also send this
text to them and ask for help filling in the gaps.

``` r
get_unknowns(dd_peng, as_request = TRUE)
#> Category labels for this variable (labels are shown in reports):
#> 
#>   - species: Adelie = ?;  Chinstrap = ?;  Gentoo = ?
#>   - sex: female = ?;  male = ?
#> 
#> Optional: additional relevant details (e.g., method of collection or measurement):
#> 
#>   - species = ?
#>   - sex = ?
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
#> 
#> Units for model output (e.g., per 10 years of age):
#> 
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
#> 
#> A label to use for this variable in reports:
#> 
#>   - species = ?
#>   - sex = ?
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
#> 
#> Variable units (e.g., age in years):
#> 
#>   - body_mass_g = ?
#>   - bill_length_mm = ?
#>   - bill_depth_mm = ?
```

### Modify dictionary values

Once we have this information ready to embed in the dictionary, we can
use `pericircles` family of `set` functions:

``` r

dd_peng <- dd_peng %>% 
  set_labels(species = "Species",
             sex = "Sex",
             bill_length_mm = "Bill length",
             bill_depth_mm = "Bill depth") %>% 
  set_category_levels(species = c("Adelie", "Chinstrap", "Gentoo"),
                      sex = c("male", "female")) %>% 
  set_category_labels(sex = c("Male", "Female")) %>% 
  set_units(bill_length_mm = "mm",
            bill_depth_mm = "mm",
            body_mass_g = "grams") %>% 
  set_divby_modeling(bill_length_mm = 5,
                     bill_depth_mm = 5)
```

#### Modifying factors

`set_category_levels()` and `set_category_labels()` are nice when you
are creating a dictionary, but `set_factor_labels()` and
`set_factor_order()` are the functions you want to use when modifying a
dictionary.

Modify factor labels, changing one or more labels in an existing
variable, with `set_factor_labels()`:

``` r

dd_peng$variables$sex
#> Nominal Variable:
#>   Name               : sex 
#>   Label              : Sex 
#>   Description        : none 
#>   Category Levels    : male, female 
#>   Category Labels    : Male, Female

dd_peng <- dd_peng %>% 
  set_factor_labels(sex = c(female = "F", male = "M"))

dd_peng$variables$sex
#> Nominal Variable:
#>   Name               : sex 
#>   Label              : Sex 
#>   Description        : none 
#>   Category Levels    : male, female 
#>   Category Labels    : M, F
```

Modify factor order, moving one or more levels to the front, with
`set_factor_order()`:

``` r

dd_peng <- dd_peng %>% 
  set_factor_order(sex = c("female"))

dd_peng$variables$sex
#> Nominal Variable:
#>   Name               : sex 
#>   Label              : Sex 
#>   Description        : none 
#>   Category Levels    : female, male 
#>   Category Labels    : F, M
```

### Apply dictionary information

Data dictionaries have a method called `recode()`, which leverages the
`dplyr::recode()` function in combination with information stored in the
dictionary to provide a ‘smart’ recoder. The ‘smart’ part is that this
recode function doesn’t require you to provide recode values. Instead,
it looks for appropriate recode values in the dictionary.

``` r

# This function is embedded in the dictionary itself, so it's called by 
# typing the name of the dictionary, followed by `$recode(x)`, where 
# `x` is the vector you intend to recode. For example:

data_peng %>% 
  mutate(sex = dd_peng$recode(sex))
#> # A tibble: 344 × 5
#>    species sex   body_mass_g bill_length_mm bill_depth_mm
#>    <fct>   <fct>       <int>          <dbl>         <dbl>
#>  1 Adelie  M            3750           39.1          18.7
#>  2 Adelie  F            3800           39.5          17.4
#>  3 Adelie  F            3250           40.3          18  
#>  4 Adelie  <NA>           NA           NA            NA  
#>  5 Adelie  F            3450           36.7          19.3
#>  6 Adelie  M            3650           39.3          20.6
#>  7 Adelie  F            3625           38.9          17.8
#>  8 Adelie  M            4675           39.2          19.6
#>  9 Adelie  <NA>         3475           34.1          18.1
#> 10 Adelie  <NA>         4250           42            20.2
#> # ℹ 334 more rows
```

This works both for variable names and variable categories. For example,
pivoting our data to a longer format will move several variable names
into a new column called `name`, and this column can be recoded just
like we did above.

``` r

data_peng %>% 
  pivot_longer(starts_with("bill_")) %>% 
  mutate(name = dd_peng$recode(name))
#> # A tibble: 688 × 5
#>    species sex    body_mass_g name        value
#>    <fct>   <fct>        <int> <chr>       <dbl>
#>  1 Adelie  male          3750 Bill length  39.1
#>  2 Adelie  male          3750 Bill depth   18.7
#>  3 Adelie  female        3800 Bill length  39.5
#>  4 Adelie  female        3800 Bill depth   17.4
#>  5 Adelie  female        3250 Bill length  40.3
#>  6 Adelie  female        3250 Bill depth   18  
#>  7 Adelie  <NA>            NA Bill length  NA  
#>  8 Adelie  <NA>            NA Bill depth   NA  
#>  9 Adelie  female        3450 Bill length  36.7
#> 10 Adelie  female        3450 Bill depth   19.3
#> # ℹ 678 more rows
```

*Warning*: I am still working out some edge cases in this method. Please
don’t hesitate to open an issue if it fails in unexpected ways. If
needed, there is a less convenient but more reliable way to get recode
information for specific parts of the dictionary:

``` r

# in case the smart dictionary recoder fails, you can fall back 
# on the more reliable methods get_variable_recoder and 
# get_level_recoder, which are also public methods for dictionaries

recode_bills <- dd_peng$get_variable_recoder(name = c("bill_length_mm",
                                                      "bill_depth_mm"))

recode_sex <- dd_peng$get_level_recoder(name = 'sex')

data_peng %>% 
  pivot_longer(starts_with("bill_")) %>% 
  mutate(name = recode(name, !!!recode_bills),
         sex = recode(sex, !!!recode_sex))
#> # A tibble: 688 × 5
#>    species sex   body_mass_g name        value
#>    <fct>   <fct>       <int> <chr>       <dbl>
#>  1 Adelie  M            3750 Bill length  39.1
#>  2 Adelie  M            3750 Bill depth   18.7
#>  3 Adelie  F            3800 Bill length  39.5
#>  4 Adelie  F            3800 Bill depth   17.4
#>  5 Adelie  F            3250 Bill length  40.3
#>  6 Adelie  F            3250 Bill depth   18  
#>  7 Adelie  <NA>           NA Bill length  NA  
#>  8 Adelie  <NA>           NA Bill depth   NA  
#>  9 Adelie  F            3450 Bill length  36.7
#> 10 Adelie  F            3450 Bill depth   19.3
#> # ℹ 678 more rows
```

### Attach all information at once

`infuse_dictionary()` puts all the relevant information from a data
dictionary into an existing dataset. This can smooth out your code when
you use packages that automatically incorporate labels into their
outputs, such as `gtsummary`. In the example below, we infuse our data
with the dictionary we created and specify that we want to format
continuous variables using their modeling units (e.g., bill length is
modeled per 10 mm). Note that when you infuse data with dictionaries and
specify `units = 'model'`, the corresponding variables will be divided
by their designated `divby_model` value and new columns will be created.
If you don’t want new columns, you can specify `divby_suffix = NULL`
when you infuse.

``` r

library(gtsummary)

data_infused <- data_peng %>% 
  infuse_dictionary(dd_peng, units = 'model', divby_suffix = NULL)

fit <- lm(formula = body_mass_g ~ ., data = data_infused)

tbl_regression(fit)
```

<div id="lbnyzyutdn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lbnyzyutdn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lbnyzyutdn thead, #lbnyzyutdn tbody, #lbnyzyutdn tfoot, #lbnyzyutdn tr, #lbnyzyutdn td, #lbnyzyutdn th {
  border-style: none;
}
&#10;#lbnyzyutdn p {
  margin: 0;
  padding: 0;
}
&#10;#lbnyzyutdn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lbnyzyutdn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#lbnyzyutdn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#lbnyzyutdn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#lbnyzyutdn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#lbnyzyutdn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lbnyzyutdn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lbnyzyutdn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#lbnyzyutdn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lbnyzyutdn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#lbnyzyutdn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#lbnyzyutdn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lbnyzyutdn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lbnyzyutdn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#lbnyzyutdn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbnyzyutdn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#lbnyzyutdn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lbnyzyutdn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lbnyzyutdn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbnyzyutdn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lbnyzyutdn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbnyzyutdn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lbnyzyutdn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbnyzyutdn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#lbnyzyutdn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lbnyzyutdn .gt_left {
  text-align: left;
}
&#10;#lbnyzyutdn .gt_center {
  text-align: center;
}
&#10;#lbnyzyutdn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lbnyzyutdn .gt_font_normal {
  font-weight: normal;
}
&#10;#lbnyzyutdn .gt_font_bold {
  font-weight: bold;
}
&#10;#lbnyzyutdn .gt_font_italic {
  font-style: italic;
}
&#10;#lbnyzyutdn .gt_super {
  font-size: 65%;
}
&#10;#lbnyzyutdn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lbnyzyutdn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lbnyzyutdn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lbnyzyutdn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lbnyzyutdn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lbnyzyutdn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lbnyzyutdn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#lbnyzyutdn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#lbnyzyutdn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label"><span class='gt_from_md'><strong>Characteristic</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="estimate"><span class='gt_from_md'><strong>Beta</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="conf.low"><span class='gt_from_md'><strong>95% CI</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="p.value"><span class='gt_from_md'><strong>p-value</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Species</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Adelie</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Chinstrap</td>
<td headers="estimate" class="gt_row gt_center">-245</td>
<td headers="conf.low" class="gt_row gt_center">-412, -79</td>
<td headers="p.value" class="gt_row gt_center">0.004</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Gentoo</td>
<td headers="estimate" class="gt_row gt_center">1,443</td>
<td headers="conf.low" class="gt_row gt_center">1,231, 1,655</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Sex</td>
<td headers="estimate" class="gt_row gt_center"><br /></td>
<td headers="conf.low" class="gt_row gt_center"><br /></td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    F</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="conf.low" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    M</td>
<td headers="estimate" class="gt_row gt_center">437</td>
<td headers="conf.low" class="gt_row gt_center">341, 534</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Bill length, per 5 mm</td>
<td headers="estimate" class="gt_row gt_center">133</td>
<td headers="conf.low" class="gt_row gt_center">61, 204</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Bill depth, per 5 mm</td>
<td headers="estimate" class="gt_row gt_center">440</td>
<td headers="conf.low" class="gt_row gt_center">241, 639</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="4"><span class='gt_from_md'>Abbreviation: CI = Confidence Interval</span></td>
    </tr>
  </tfoot>
  &#10;</table>
</div>

## Analysis helpers

### Summarize overall and in each group, separately.

We often want to summarize results overall and in subgroups based on
multiple grouping variables. The `dplyr::summarize` function is almost
perfect for this, but it isn’t designed to let you summarize overall and
within multiple subgroups in one call. That’s why we include
`summarize_each_group` in `pericircle` - it lets you run
`dplyr::summarize` in all the groups of interest, without duplicating
code.

``` r

data_peng %>% 
  summarize_each_group(mean_weight = mean(body_mass_g, na.rm = TRUE),
                       nobs = n(),
                       groups = c("species", "sex"))
#> # A tibble: 7 × 4
#>   .group_variable .group_level mean_weight  nobs
#>   <chr>           <chr>              <dbl> <int>
#> 1 .overall        .overall           4202.   344
#> 2 species         Adelie             3701.   152
#> 3 species         Chinstrap          3733.    68
#> 4 species         Gentoo             5076.   124
#> 5 sex             female             3862.   165
#> 6 sex             male               4546.   168
#> 7 sex             <NA>               4006.    11
```

Note that `summarize_each_group` works with grouped data too, but it
does not nest the groups. If you are hoping to summarize groups within
groups, `summarize_each_group` is not the tool to use.

``` r

data_peng %>% 
  group_by(species, sex) %>% 
  summarize_each_group(mean_weight = mean(body_mass_g, na.rm = TRUE),
                       nobs = n())
#> # A tibble: 7 × 4
#>   .group_variable .group_level mean_weight  nobs
#>   <chr>           <chr>              <dbl> <int>
#> 1 .overall        .overall           4202.   344
#> 2 species         Adelie             3701.   152
#> 3 species         Chinstrap          3733.    68
#> 4 species         Gentoo             5076.   124
#> 5 sex             female             3862.   165
#> 6 sex             male               4546.   168
#> 7 sex             <NA>               4006.    11
```

But if you *really* want to use `summarize_each_group()` with nested
groups, here’s one way to do that:

``` r

data_peng %>% 
  split(.$species) %>% 
  map_dfr(
    ~ .x %>% 
      group_by(sex) %>% 
      summarize_each_group(mean_weight = mean(body_mass_g, na.rm = TRUE),
                           nobs = n()),
    .id = 'species'
  )
#> # A tibble: 11 × 5
#>    species   .group_variable .group_level mean_weight  nobs
#>    <chr>     <chr>           <chr>              <dbl> <int>
#>  1 Adelie    .overall        .overall           3701.   152
#>  2 Adelie    sex             female             3369.    73
#>  3 Adelie    sex             male               4043.    73
#>  4 Adelie    sex             <NA>               3540      6
#>  5 Chinstrap .overall        .overall           3733.    68
#>  6 Chinstrap sex             female             3527.    34
#>  7 Chinstrap sex             male               3939.    34
#>  8 Gentoo    .overall        .overall           5076.   124
#>  9 Gentoo    sex             female             4680.    58
#> 10 Gentoo    sex             male               5485.    61
#> 11 Gentoo    sex             <NA>               4588.     5
```
