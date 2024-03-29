% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_for_authentication}
\alias{check_for_authentication}
\title{Review API authentication}
\usage{
check_for_authentication(user = NULL, pass = NULL)
}
\arguments{
\item{user}{A DHIS2 user account to authenticate.}

\item{pass}{Password of the DHIS2 Account.}
}
\value{
A list with API key (user and pass) if found.
}
\description{
Check that the API pass key is specified. It looks for the API key from the
\code{r} \code{environment}, if not supplied, otherwise throws an error.
}
