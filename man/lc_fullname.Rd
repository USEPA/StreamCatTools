% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lc_get_params.R
\name{lc_fullname}
\alias{lc_fullname}
\title{Lookup Full Metric Name}
\usage{
lc_fullname(metric = NULL)
}
\arguments{
\item{metric}{Short metric name
Syntax: metric=value1
Values: metric}
}
\value{
A lookup of the full name for a given LakeCat metric
}
\description{
Function to retrieve a full metric name based on the short name using the LakeCat API.
}
\examples{
fullname <- lc_fullname(metric='clay')
}
\author{
Marc Weber
}
