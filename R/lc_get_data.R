#' @title Get LakeCat data
#'
#' @description
#' Function to return LakeCat metrics using the StreamCat API.  The function allows a user to get
#' specific metric data aggregated by area of interest, returned by comid(s), hydroregion(s), state(s), or county(ies).
#'
#' @author
#' Marc Weber
#'
#' @param metric Name(s) of metrics to query. Must be character string with comma-delimited list of metrics. \emph{\strong{Not} case-sensitive}.
#' Syntax: name=<name1>,<name2>
#'
#' @param aoi Specify the area of interest described by a metric. By default, all available areas of interest
#' for a given metric are returned. \emph{Case-sensitive}.
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: catchment|watershed|
#'
#' @param comid Return metric information for specific COMIDs. Can be a comma-delimited list, a character vector,
#' or any object that can be coerced to a comma-delimited list with \code{\link[base]{paste}}. 
#' One of \code{comid}, \code{county}, \code{state}, or \code{region} is required unless \code{conus='true'}.
#' Syntax: comid=<comid1>,<comid2>
#' 
#' @param state Return metric information for COMIDs within a specific state. Use a state's abbreviation to
#' query for a given state. One of \code{comid}, \code{county}, \code{state}, or \code{region} is required unless \code{conus='true'}. 
#' If specified \emph{and valid}, \code{comid} and \code{county} are ignored. \emph{Case-sensitive}.
#' Syntax: state=<state1>,<state2>
#'
#' @param county Return metric information for COMIDs within a specific county.
#' Users must use the FIPS code, not county name, as a way to disambiguate counties.
#' One of \code{comid}, \code{county}, \code{state}, or \code{region} is required unless \code{conus='true'}. If specified \emph{and valid}, \code{comid} is ignored.
#' Syntax: county=<county1>,<county1>
#'
#' @param region Return metric information for COMIDs within a specified hydroregion.
#' Hydroregions are specified using full name i.e. \code{'Region01'}, \code{'Region03N'}, \code{'Region10L'}
#' One of \code{comid}, \code{county}, \code{state}, or \code{region} is required unless \code{conus='true'}.  
#' If specified \emph{and valid}, \code{comid}, \code{county}, and \code{state} are ignored. \emph{Case-sensitive}.
#' Syntax: region=<regionid1>,<regionid2>
#'
#' @param conus Return all COMIDs in the conterminous United States. Character string (\emph{\strong{Not} case-sensitive}) or logical.
#' The default value is false. If true, \code{comid}, \code{county}, \code{state}, and \code{region} are ignored.
#' Values: true|false
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
#' @return A tibble of desired StreamCat metrics. If data are missing for all rows of a given metric, then the column for that metric will not exist. If data are missing for only some rows, then they will be specified with NA.
#'
#' @examples
#' \donttest{
#' df <- lc_get_data(comid='23794487', aoi='cat', metric='fert')
#'
#' df <- lc_get_data(metric='pcturbmd2006', aoi='ws',
#' comid='24083377')
#'
#' df <- lc_get_data(metric='pctgrs2006', aoi='ws', region='Region01')
#' 
#' df <- lc_get_data(metric='pctwdwet2006', aoi='ws', county='41003')
#' 
#' df <- lc_get_data(metric='pcturbmd2006', aoi='ws',
#' comid='24083377', showAreaSqKm=FALSE, showPctFull=TRUE)
#'
#' df <- lc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='23783629,23794487,23812618')
#'
#' df <- lc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid=c('23783629','23794487','23812618'))
#' 
#' df <- lc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='23783629,23794487,23812618',
#' countOnly=TRUE)
#' 
#'
#'  }
#' @export

lc_get_data <- function(comid = NULL,
                        metric = NULL,
                        aoi = NULL,
                        showAreaSqKm = NULL,
                        showPctFull = NULL,
                        state = NULL,
                        county = NULL,
                        region = NULL,
                        conus = NULL,
                        countOnly = NULL) {
  # Base API URL.
  req <- httr2::request('https://api.epa.gov/StreamCat/lakes/metrics')
  # Collapse comids into a single string separated by a comma.
  if (!is.null(comid)){
    comid <- paste(comid, collapse = ",")
    if (length(strsplit(comid, ",")[[1]]) > 700){
      comids_split <- split(comid, ceiling(seq_along(comid)/700))
    }
  }
  # Force old and odd naming convention to behave correctly
  if (!is.null(aoi)){
    if (aoi == 'catchment') aoi <- 'cat'
    if (aoi == 'watershed') aoi <- 'ws'
  }
  if ((is.null(comid) & is.null(state) & is.null(county) & is.null(region) & is.null(conus)) | is.null(metric) | is.null(aoi)){
    stop('Must provide at a minimum valid comid, metric and aoi to the function')
  }
  if (!is.null(conus) & metric=='all'){
    stop('If you are requesting all metrics please request for regions, states or counties rather than all of conus')
  } 
  if (metric=='all'){
    message("Using metric='all' with a large aoi may take a considerable amount of time to return results - request may timeout if multiple AOIs are requested")
  }
  metric = tolower(metric)
  items = unlist(strsplit(metric,','))
  items = gsub(" ","",items)
  items = gsub("\n","",items)
  params <- sc_get_params(param='metric_names')
  if (metric != 'all' & !all(items %in% params)){
    message("One or more of the provided metric names do not match the expected metric names in StreamCat.  Use sc_get_params(param='name') to list valid metric names for StreamCat")
  }
  metric = tolower(metric)
  items = unlist(strsplit(metric,','))
  items = gsub(" ","",items)
  items = gsub("\n","",items)
  params <- sc_get_params(param='metric_names')
  if (metric != 'all' & !all(items %in% params)){
    message("One or more of the provided metric names do not match the expected metric names in StreamCat.  Use sc_get_params(param='name') to list valid metric names for StreamCat")
  }
  if (exists('comid_split')){
    create_post_request <- function(comids) {
      df <- req |>
        httr2::req_method("POST") |>
        httr2::req_headers(comid=comids,aoi=aoi,name=metric,showareasqkm=showAreaSqKm,
                           showpctfull=showPctFull,state=state,county=county,region=region,
                           conus=conus,countOnly=countOnly) |>
        httr2::req_method("POST") |>
        httr2::req_throttle(rate = 30 / 60) |> 
        httr2::req_retry(backoff = ~ 5, max_tries = 3) |>  
        httr2::req_perform() |> 
        httr2::resp_body_string() |> 
        jsonlite::fromJSON() 
      # Return a data frame
      if (is.null(countOnly)){
        df <- df$items  |> 
          dplyr::select(comid, dplyr::everything())
        return(df)
      } else return(df$items)
    }
    # Create a list of requests using purrr::map()
    df <- purrr::map_dfr(comids_split, create_post_request)
    return(df)
    
  } else {
    df <- req |>
      httr2::req_method("POST") |>
      httr2::req_headers(comid=comid,aoi=aoi,name=metric,showareasqkm=showAreaSqKm,
                         showpctfull=showPctFull,state=state,county=county,region=region,
                         conus=conus,countOnly=countOnly) |>
      httr2::req_method("POST") |>
      httr2::req_throttle(rate = 30 / 60) |> 
      httr2::req_retry(backoff = ~ 5, max_tries = 3) |>  
      httr2::req_perform() |> 
      httr2::resp_body_string() |> 
      jsonlite::fromJSON()
    # Return a data frame
    if (is.null(countOnly)){
      df <- df$items  |> 
        dplyr::select(comid, dplyr::everything())
      return(df)
    } else return(df$items)
  }
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
