% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_latest_events.R
\name{generate_uid}
\alias{generate_uid}
\title{Generate a unique DHIS2 type ID}
\usage{
generate_uid(code_size = 11)
}
\arguments{
\item{code_size}{Integer, size of the uid. default 14.}
}
\value{
A character string, the unique ID
}
\description{
Randomly generates a unique DHIS2 uid.
}
