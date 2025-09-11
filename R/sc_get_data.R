#' @title Get StreamCat data
#'
#' @description
#' Function to return StreamCat catchment and watershed metrics using the StreamCat API.  The function allows a user to get
#' specific metric data aggregated by area of interest, returned by comid(s), hydroregion(s), state(s), or county(ies).
#'
#' @author
#' Marc Weber
#'
#' @param metric Name(s) of metrics to query. Must be character string with comma-delimited list of metrics,
#' or, if \code{metric='all'} then all metrics will be queried. \emph{\strong{Not} case-sensitive}.
#' Syntax: name=<name1>,<name2>
#'
#' @param aoi Name(s) of areas of interest to query.
#' If a metric does not have data for a given AOI, no data is returned for that AOI.
#' Certain metrics that have no AOI specified for StreamCat need the AOI to be specified as \code{'other'}. These
#' metrics include: BankfullDepth, BankfullWidth, ThalwagDepth (sic), CHEM_V2_1, CONN, HABT, HYD, ICI, IWI, TEMP, WettedWidth,
#' prg_bmmi, and all the mast, msst, mwst metrics. \emph{Case-sensitive}. 
#'
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: cat|ws|catrp100|wsrp100|other
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
#' @return A data frame of StreamCat metrics. If data are missing for all rows of a given metric, then the column for that metric will not exist. If data are missing for only some rows, then they will be specified with NA.
#'
#' @examples
#' \donttest{
#' df <- sc_get_data(comid='179', aoi='cat', metric='fert')
#'
#' df <- sc_get_data(metric='pctgrs2006', aoi='ws', region='Region01')
#'
#' df <- sc_get_data(metric='pctwdwet2006', aoi='ws', county='41003')
#'
#' df <- sc_get_data(metric='pcturbmd2006', aoi='wsrp100',
#' comid='1337420')
#'
#' df <- sc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='179,1337,1337420')
#'
#' df <- sc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='179,1337,1337420',
#' showAreaSqKm='true', showPctFull='true')
#'
#' df <- sc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='179,1337,1337420', countOnly='true')
#'
#' df <- sc_get_data(metric='thalwagdepth', comid='179,1337,1337420', aoi='other')
#' 
#' df <- sc_get_data(metric='thalwagdepth', comid=c('179','1337','1337420'), aoi='other')
#' 
#' df <- sc_get_data(comid='179', aoi='ws', metric='all')
#'  }
#' @export

sc_get_data <- function(comid = NULL,
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
  base_url <- 'https://api.epa.gov/StreamCat/streams/metrics'
  req <- httr2::request(base_url)
  # Collapse comids into a single string separated by a comma.
  if (!is.null(comid)){
    comid <- paste(comid, collapse = ",")
    if (length(strsplit(comid, ",")[[1]]) > 700){
      chunk_size <- 700
      group_factor <- ceiling(seq_along(strsplit(comid, ",")[[1]]) / chunk_size)
      comids_split <- split(strsplit(comid, ",")[[1]], group_factor)
    }
  }
  # Force old and odd naming convention to behave correctly
  if (!is.null(aoi)){
      if (stringr::str_detect(aoi,'catchment')) {
        aoi <- gsub('catchment','cat',aoi)
      }
      if (stringr::str_detect(aoi,'watershed')) {
        aoi <- gsub('watershed','ws',aoi)
      }
      if (stringr::str_detect(aoi,'riparian_catchment')) {
        aoi <- gsub('riparian_catchment','catrp100',aoi)
      }
      if (stringr::str_detect(aoi,'riparian_watershed')) {
        aoi <- gsub('riparian_watershed','wsrp100',aoi)
      }
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
    message("One or more of the provided metric names do not match the expected metric names in StreamCat.  Use sc_get_params(param='metric_names') to list valid metric names for StreamCat")
  }
  if (exists('comids_split')){
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
    res <- lapply(comids_split, create_post_request)
    df <- bind_rows(res)
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
#' Function to retrieve all NLCD metrics for a given year using the StreamCat API.
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
#' @param state Return metric information for COMIDs within a specific state. Use a state's abbreviation to
#' query for a given state.
#' Syntax: state=<state1>,<state2>
#'
#' @param county Return metric information for COMIDs within a specific county.
#' Users must use the FIPS code, not county name, as a way to disambiguate counties.
#' Syntax: county=<county1>,<county1>
#'
#' @param region Return metric information for COMIDs within a specified hydroregion.
#' Syntax: region=<regionid1>,<regionid2>
#'
#' @param conus Return all COMIDs in the conterminous United States.
#' The default value is false.
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
#' @return A tibble of desired StreamCat metrics
#'
#' @examples
#' \donttest{
#' df <- sc_nlcd(year='2001', aoi='cat',comid='179,1337,1337420')
#'
#' df <- sc_nlcd(year='2001', aoi='ws', region='Region01')
#'
#' df <- sc_nlcd(year='2001', aoi='ws', region='Region01',
#' countOnly=TRUE)
#'
#' df <- sc_nlcd(year='2001', aoi='ws', region='Region01',
#' showAreaSqKm=FALSE, showPctFull=TRUE)
#'
#' df <- sc_nlcd(year='2001, 2006', aoi='cat,ws',
#' comid='179,1337,1337420')
#' }
#' @export

sc_nlcd <- function(year = '2019',
                    comid = NULL,
                    aoi = NULL,
                    showAreaSqKm = NULL,
                    showPctFull = NULL,
                    state = NULL,
                    county = NULL,
                    region = NULL,
                    conus = NULL,
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
  metric <- paste0(all_comb$Var1,
                      all_comb$Var2,
                      collapse = ",",
                      recycle0 = TRUE)
  # Query the API.
  final_df <- sc_get_data(
    metric = metric,
    aoi = aoi,
    comid = comid,
    state = state,
    county = county,
    region = region,
    showAreaSqKm = showAreaSqKm,
    showPctFull = showPctFull,
    conus = conus,
    countOnly = countOnly
  )
  # End of function. Return a data frame.
  return(final_df)
}

ignore_unused_imports <- function() {
  curl::curl_parse_url()
}
