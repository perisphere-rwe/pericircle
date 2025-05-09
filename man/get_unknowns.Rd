% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_unknowns}
\alias{get_unknowns}
\title{Get Unknown Fields in Dictionary}
\usage{
get_unknowns(x, as_request = FALSE)
}
\arguments{
\item{x}{A \link[=data_dictionary]{data dictionary} object.}

\item{as_request}{logical. If \code{TRUE}, text is returned in a readable
format that can be pasted into other settings, e.g., an e-mail
requesting for the unknown information. If \code{FALSE} (the default),
unknowns are returned as a \code{tibble}.}
}
\value{
If \code{as_request = FALSE}, a \link[tibble:tibble-package]{tibble}.
If \code{as_request = TRUE}, text is returned describing the unknowns.
}
\description{
Get Unknown Fields in Dictionary
}
\examples{

# units isn't returned in the output because it is specified (known)
get_unknowns(data_dictionary(numeric_variable("a", units = 'years')))

# label isn't returned in the output because it is specified (known)
get_unknowns(data_dictionary(nominal_variable("b", label = 'example')))
# as_request = TRUE returns this information in a shareable format
as_data_dictionary(iris) \%>\%
  set_labels(Sepal.Length = "Sepal length",
            Sepal.Width = "Sepal width",
            Species = "Flower species") \%>\%
  set_descriptions(Species = "The species are all subtypes of iris") \%>\%
  set_units(Sepal.Length = "cm") \%>\%
  get_unknowns(as_request = TRUE)
}
