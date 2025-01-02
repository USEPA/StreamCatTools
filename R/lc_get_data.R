#' @title Get LakeCat data
#'
#' @description
#' Function to return LakeCat metrics using the StreamCat API.  The function allows a user to get
#' specific metric data aggregated by area of interest, returned by comid(s), hydroregion(s), state(s), or county(ies).
#'
#' @author
#' Marc Weber
#'
#' @param metric Name(s) of metrics to query
#' Syntax: name=<name1>,<name2>
#'
#' @param aoi Specify the area of interest described by a metric. By default, all available areas of interest
#' for a given metric are returned.
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: catchment|watershed|
#'
#' @param comid Return metric information for specific COMIDs.  Needs to be a character string
#' and function will convert to this format if needed.
#' Syntax: comid=<comid1>,<comid2>
#'
#' @param showAreaSqKm Return the area in square kilometers of a given area of interest.
#' The default value is false.
#' Values: true|false
#'
#' @param showPctFull Return the pctfull for each dataset. The default value is false.
#' Values: true|false
#'
#' @param countOnly Return a CSV containing only the row count (ROWCOUNT) and the column
#' count (COLUMNCOUNT) that the server expects to return in a request. The default value is false.
#' Values: true|false
#'
#' @return A tibble of desired StreamCat metrics
#'
#' @examples
#' \donttest{
#' df <- lc_get_data(comid='23794487', aoi='cat', metric='fert')
#'
#' df <- lc_get_data(metric='pcturbmd2006', aoi='ws',
#' comid='24083377')
#'
#' df <- lc_get_data(metric='pcturb,d2006', aoi='ws',
#' comid='24083377', showAreaSqKm=FALSE, showPctFull=TRUE)
#'
#' df <- lc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='23783629,23794487,23812618')
#'
#' df <- lc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='23783629,23794487,23812618',
#' countOnly=TRUE)
#'
#'  }
#' @export

lc_get_data <- function(metric = NULL,
                        aoi = NULL,
                        comid = NULL,
                        showAreaSqKm = NULL,
                        showPctFull = NULL,
                        countOnly = NULL) {
  # Base API URL.
  req <- httr2::request('https://api.epa.gov/StreamCat/lakes/metrics')
  # Collapse comids into a single string separated by a comma.
  if (!is.null(comid))
    comid <- paste(comid, collapse = ",")
  # Force old and odd naming convention to behave correctly
  if (!is.null(aoi)){
    if (aoi == 'catchment') aoi <- 'cat'
    if (aoi == 'watershed') aoi <- 'ws'
  }
  df <- req |>
    httr2::req_method("POST") |>
    httr2::req_headers(comid=comid,aoi=aoi,name=metric,showareasqkm=showAreaSqKm,
                       showpctfull=showPctFull,countOnly=countOnly) |>
    httr2::req_perform() |> 
    # extract response body as string
    httr2::resp_body_string() |> 
    jsonlite::fromJSON()
  # End of function. Return a data frame.
  return(df$items)
}

#' @title Get NLCD Data
#'
#' @description
#' Function to specifically retrieve all NLCD metrics for a given year using the StreamCat API.
#'
#' @author
#' Marc Weber
#'
#' @param year Years(s) of NLCD metrics to query.
#' Only valid NLCD years are accepted (i.e. 2001, 2004, 2006, 2008,
#' 2011, 2013, 2016, 2019)
#' Syntax: year=<year1>,<year2>
#'
#' @param aoi Specify the area of interest described by a metric. By default, all available areas of interest
#' for a given metric are returned.
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: catchment|watershed|riparian_catchment|riparian_watershed|other
#'
#' @param comid Return metric information for specific COMIDs
#' Syntax: comid=<comid1>,<comid2>
#'
#' @param showAreaSqKm Return the area in square kilometers of a given area of interest.
#' The default value is false.
#' Values: true|false
#'
#' @param showPctFull Return the pctfull for each dataset. The default value is false.
#' Values: true|false
#'
#' @param countOnly Return a CSV containing only the row count (ROWCOUNT) and the column
#' count (COLUMNCOUNT) that the server expects to return in a request. The default value is false.
#' Values: true|false
#'
#' @return A tibble of desired StreamCat metrics
#'
#' @examples
#' \donttest{
#'
#' df <- lc_nlcd(comid='23783629', year='2019', aoi='ws')
#'
#' df <- lc_nlcd(year='2016', aoi='cat',
#' comid='23783629,23794487,23812618', showAreaSqKm=FALSE, showPctFull=TRUE)
#'
#' df <- lc_nlcd(year='2016', aoi='cat',
#' comid='23783629,23794487,23812618', countOnly=TRUE)
#'
#' df <- lc_nlcd(year='2016, 2019', aoi='cat,ws',
#' comid='23783629,23794487,23812618')
#' }
#' @export


lc_nlcd <- function(year = '2019', aoi = NULL, comid = NULL,
                    showAreaSqKm = NULL, showPctFull = NULL,
                    countOnly = NULL) {
  # year must be a character string.
  year_chr <-  as.character(year)
  # split multiple years supplied as a single string into
  # a vector of years.
  year_vec <- unlist(strsplit(x = year_chr,
                              split = ",|, "))
  # Vector of valid NLCD years to check inputs against.
  valid_years <- c('2001',
                   '2004',
                   '2006',
                   '2008',
                   '2011',
                   '2013',
                   '2016',
                   '2019')
  # Stop early if any of the year(s) supplied are not found in the valid
  # years vec.
  stopifnot(
    "year must be a valid NLCD land cover year: 2001, 2004,
         2006, 2008, 2011, 2013, or 2019" = any(year_vec %in% valid_years)
  )
  # Vector of NLCD metric names.
  nlcd <- c(
    'pctmxfst',
    'pctow',
    'pctshrb',
    'pcturbhi',
    'pcturblo',
    'pcturbmd',
    'pcturbop',
    'pctwdwet',
    'pctbl',
    'pctconif',
    'pctcrop',
    'pctdecid',
    'pctgrs',
    'pcthay',
    'pcthbwet',
    'pctice'
  )
  # Create a data frame of all NLCD Metric and year combinations.
  all_comb <- expand.grid(nlcd, year_vec)
  # Concatenate the NLCD metric name with the supplied year(s) to create
  # valid metric names to submit to the API.
  nlcd_mets <- paste0(all_comb$Var1,
                      all_comb$Var2,
                      collapse = ",",
                      recycle0 = TRUE)
  # Query the API.
  final_df <- lc_get_data(
    metric = nlcd_mets,
    aoi = aoi,
    comid = comid,
    showAreaSqKm = showAreaSqKm,
    showPctFull = showPctFull,
    countOnly = countOnly
  )
  # End of function. Return a data frame.
  return(final_df)
}
