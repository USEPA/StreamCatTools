% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lc_get_data.R
\name{lc_get_data}
\alias{lc_get_data}
\title{Get LakeCat data}
\usage{
lc_get_data(
  metric = NULL,
  aoi = NULL,
  comid = NULL,
  showAreaSqKm = NULL,
  showPctFull = NULL,
  countOnly = NULL
)
}
\arguments{
\item{metric}{Name(s) of metrics to query
Syntax: name=<name1>,<name2>}

\item{aoi}{Specify the area of interest described by a metric. By default, all available areas of interest
for a given metric are returned.
Syntax: areaOfInterest=<value1>,<value2>
Values: catchment|watershed|}

\item{comid}{Return metric information for specific COMIDs.  Needs to be a character string
and function will convert to this format if needed.
Syntax: comid=<comid1>,<comid2>}

\item{showAreaSqKm}{Return the area in square kilometers of a given area of interest.
The default value is false.
Values: true|false}

\item{showPctFull}{Return the pctfull for each dataset. The default value is false.
Values: true|false}

\item{countOnly}{Return a CSV containing only the row count (ROWCOUNT) and the column
count (COLUMNCOUNT) that the server expects to return in a request. The default value is false.
Values: true|false}
}
\value{
A tibble of desired StreamCat metrics
}
\description{
Function to return LakeCat metrics using the StreamCat API.  The function allows a user to get
specific metric data aggregated by area of interest, returned by comid(s), hydroregion(s), state(s), or county(ies).
}
\examples{
\donttest{
df <- lc_get_data(comid='23794487', aoi='catchment', metric='fert')

df <- lc_get_data(metric='PctUrbMd2006', aoi='watershed',
comid='24083377')

df <- lc_get_data(metric='PctUrbMd2006', aoi='watershed',
comid='24083377', showAreaSqKm=FALSE, showPctFull=TRUE)

df <- lc_get_data(metric='PctUrbMd2006,DamDens',
aoi='catchment,watershed', comid='23783629,23794487,23812618')

df <- lc_get_data(metric='PctUrbMd2006,DamDens',
aoi='catchment,watershed', comid='23783629,23794487,23812618',
countOnly=TRUE)

 }
}
\author{
Marc Weber
}
