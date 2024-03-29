% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lc_get_params.R
\name{lc_get_params}
\alias{lc_get_params}
\title{Get LakeCat Parameters}
\usage{
lc_get_params(param = NULL)
}
\arguments{
\item{param}{Either name or area to grab JSON of parameters in API
Syntax: param=<value1>,<value2>
Values: name|area}
}
\value{
A list of all the current LakeCat values for a given parameter
}
\description{
Function to return available LakeCat parameters using the StreamCat API.
}
\examples{
params <- lc_get_params(param='name')
params <- lc_get_params(param='areaOfInterest')
}
\author{
Marc Weber
}
