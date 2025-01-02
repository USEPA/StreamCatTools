#' @title Get StreamCat data
#'
#' @description
#' Function to return StreamCat catchment and watershed metrics using the StreamCat API.  The function allows a user to get
#' specific metric data aggregated by area of interest, returned by comid(s), hydroregion(s), state(s), or county(ies).
#'
#' @author
#' Marc Weber
#'
#' @param metric Name(s) of metrics to query
#' Syntax: name=<name1>,<name2>
#'
#' @param aoi Specify the area of interest described by a metric. For riparian area (catrp100 and wsrp100)
#' if the metric does not have data for the riparian area, no data is returned for that AOI.
#' Certain metrics have no AOI specified for StreamCat so AOI needs to be left null.  These
#' metrics are: BankfullDepth, BankfullWidth, CHEM_V2_1, CONN, HABT, HYD, ICI, IWI, TEMP, WettedWidth,
#' prg_bmmi, and all the mast, msst, mwst metrics
#'  
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: cat|ws|catrp100|wsrp100
#'
#' @param comid Return metric information for specific COMIDs.  Needs to be a character string
#' and function will convert to this format if needed.
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
#' Hydroregions are specified using full name i.e. 'Region01', 'Region03N', 'Region10L' 
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
#' @return A data frame of StreamCat metrics
#'
#' @examples
#' \donttest{
#' df <- sc_get_data(comid='179', aoi='cat', metric='fert')
#'
#' df <- sc_get_data(metric='pctgrs2006', aoi='ws', region='Region01')
#'
#' df <- sc_get_data(metric='pcturbmd2006', aoi='wsrp100',
#' comid='1337420')
#'
#' df <- sc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='179,1337,1337420')
#'
#' df <- sc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='179,1337,1337420',
#' showAreaSqKm=TRUE, showPctFull=TRUE)
#'
#' df <- sc_get_data(metric='pcturbmd2006,damdens',
#' aoi='cat,ws', comid='179,1337,1337420', countOnly='true')
#'
#' df <- sc_get_data(metric='ThalwagDepth', comid='179,1337,1337420',aoi='other')
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
  req <- httr2::request('https://api.epa.gov/StreamCat/streams/metrics')
  # Collapse comids into a single string separated by a comma.
    if (!is.null(comid))
    comid <- paste(comid, collapse = ",")
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
    if ((is.null(comid) & is.null(state) & is.null(county) & is.null(region) & is.null(conus)) | is.null(metric) |is.null(aoi)){
      stop('Must provide at a minimum valid comid, metric and aoi to the function')
    }
    items = unlist(strsplit(metric,','))
    items = gsub(" ","",items)
    items = gsub("\n","",items)
    params <- sc_get_params(param='name')
    if (!all(items %in% params)){
      stop("One or more of the provided metric names do not match the expected metrics names in StreamCat.  Use sc_get_params(param='name') to list valid metric names for StreamCat")
    }
  df <- req |>
    httr2::req_method("POST") |>
    httr2::req_headers(comid=comid,aoi=aoi,name=metric,showareasqkm=showAreaSqKm,
                       showpctfull=showPctFull,state=state,county=county,region=region,
                       conus=conus,countOnly=countOnly) |>
    httr2::req_perform() |> 
    # extract response body as string
    httr2::resp_body_string() |> 
    jsonlite::fromJSON()
  # End of function. Return a data frame.
  if (is.null(countOnly)){
    df <- df$items  |> 
      dplyr::select(comid, dplyr::everything())
    return(df)
  } else return(df$items)
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
#' df <- sc_nlcd(comid='1337420', year='2001', aoi='ws', region='Region01')
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


sc_nlcd <- function(year = '2019', aoi = NULL, comid = NULL, state = NULL,
                    county = NULL, region = NULL, showAreaSqKm = NULL,
                    showPctFull = NULL, conus = NULL, countOnly = NULL) {
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
  final_df <- sc_get_data(
    metric = nlcd_mets,
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
