% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xml_mining_functions.R
\name{xml_to_tibble}
\alias{xml_to_tibble}
\title{Create tibble from xml files}
\usage{
xml_to_tibble(folder = NULL, file = NULL, n_head = NA, primary = FALSE)
}
\arguments{
\item{folder}{specify folder form which all .xml files are used}

\item{file}{specify single file to load}

\item{n_head}{limit output to first n_head grading reports (passed to get_data_from_xml)}

\item{primary}{logical; are we reading primary grading results? (primary = FALSE == variability reports. primary = TRUE == primary grading reports.)}
}
\description{
Takes a path to a folder and returns a tibble with all grading reports from all .xml files at
the specified folder. If n_head is NA (default), all gradingreports are included. If n_head is a positive number,
only n_head gradingreports from each .xml file in folder 'path' is included.
}
