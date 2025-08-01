% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataDictionary.R
\name{data_dictionary}
\alias{data_dictionary}
\title{Create Data Dictionary from Variable Definitions}
\usage{
data_dictionary(..., variable_list = NULL)
}
\arguments{
\item{...}{One or more objects inheriting from A \link[=numeric_variable]{numeric variable}
or A \link[=nominal_variable]{nominal variable}.}

\item{variable_list}{A list of data variables. This argument allows
\code{data_dictionary} to be used programmatically and is optional. It is
intended to be used as an alternative to \code{...}.}
}
\value{
A \code{DataDictionary} object containing a tibble summary of all
variables.
}
\description{
Initializes a \code{DataDictionary} object from a set of variables.
Variables should be made using \code{\link[=numeric_variable]{numeric_variable()}} or \code{\link[=nominal_variable]{nominal_variable()}}
and passed directly as arguments.
}
\details{
if both \code{...} and \code{variable_list} are specified, the dictionary
will only be created using \code{variable_list}.
}
\examples{

age_years <- numeric_variable(
  name = "age",
  label = "Age of participant",
  units = "years"
)

gender <- nominal_variable(
  name = "gender",
  label = "Gender of participant",
  category_levels = c("M", "F"),
  category_labels = c("Male", "Female")
)

dd <- data_dictionary(age_years, gender)
print(dd)

}
