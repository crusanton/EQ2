% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{eq_create_label}
\alias{eq_create_label}
\title{Create label for HTML}
\usage{
eq_create_label(input_data)
}
\arguments{
\item{input_data}{Earthquake data}
}
\value{
Earthquake data Clean
}
\description{
Create label for HTML
}
\examples{
\dontrun{
file_name <- system.file("eq_data", "extdata/earthquakes.txt", package = "EQ")
eq_data   <- file_name                                                           \%>\%
             eq_clean_data()                                                     \%>\%
             dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)  \%>\%
             dplyr::mutate(popup_text = eq_create_label(.))                      \%>\%
             eq_map(annot_col = "popup_text")
}
}
