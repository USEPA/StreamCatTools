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
#' \dontrun{
#' df <- sc_get_data(comid='179', aoi='cat', metric='fert')
#'
#' df <- sc_get_data(metric='pctgrs2006', aoi='ws', region='Region01')
#'
#' df <- sc_get_data(metric='pctwdwet2006', aoi='ws', county='41003')
#'
#' df <- sc_get_data(metric='pcturbmd2006', aoi='ws,rp100',
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
  if ((is.null(comid) & is.null(state) & is.null(county) & is.null(region) & is.null(conus)) | is.null(metric) | is.null(aoi)){
    stop('Must provide at a minimum valid comid, metric and aoi to the function')
  }
  # Collapse vectors into a single string separated by a comma.
  if (!is.null(comid)){
    # This handles numeric vectors, character vectors, lists, or "100,200,300" strings
    comid_vec <- .parse_comids(comid)
    
    # Use your internal offline validator against the LakeCat COMID set (RDS from S3)
    # Will stop() with a helpful message if any are invalid
    is_valid_comid(comid_vec)
    
    # Collapse to the API's comma-separated form
    comid <- paste(comid_vec, collapse = ",")
  }
  # Collapse other vectors into comma-separated strings
  metric <- paste(metric, collapse = ",")
  aoi <- paste(aoi, collapse = ",")
  if (!is.null(state)){
    state <- paste(state, collapse = ",")
  }
  if (!is.null(county)){
    county <- paste(county, collapse = ",")
  }
  if (!is.null(region)){
    region <- paste(region, collapse = ",")
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
  header_data <- list(comid=comid,aoi=aoi,name=metric,
                      showareasqkm=showAreaSqKm,showpctfull=showPctFull,
                      state=state,county=county,region=region,conus=conus,
                      countOnly=countOnly
  )
  df <- tryCatch({
    req |>
    httr2::req_method("POST") |>
    httr2::req_headers("Content-Type" = "application/x-www-form-urlencoded") |>
    httr2::req_method("POST") |>
    httr2::req_body_form(!!!header_data) |> 
    httr2::req_throttle(rate = 30 / 60) |> 
    httr2::req_retry(backoff = ~ 5, max_tries = 3) |>  
    httr2::req_perform() |> 
    httr2::resp_body_string() |> 
    jsonlite::fromJSON()
  },error = function(e) {
    message("An error occurred during req_perform(); the service may be down or function parameters may be mis-specified: ", e$message)
    return(NULL)
  })
  # Return a data frame if success
  if (exists("df") && !is.null(df)){
    # Return a data frame
      if (is.null(countOnly)){
        df <- df$items  |> 
          dplyr::select(comid, dplyr::everything())
        return(df)
      } else return(df$items)
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
#' `r lifecycle::badge("deprecated")`
#' `sc_nlcd()` was renamed to `sc_get_nlcd()` to create a more consistent API.
#' Function to retrieve all NLCD metrics for a given year using the StreamCat API.
#'
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
#' \dontrun{
#' 
#' df <- sc_get_nlcd(year='2001', aoi='cat',comid='179') # Will show a deprecation warning
#' 
#' df <- sc_get_nlcd(year='2001', aoi='cat',comid='179,1337,1337420')
#'
#' df <- sc_get_nlcd(year='2001', aoi='ws', region='Region01')
#'
#' df <- sc_get_nlcd(year='2001', aoi='ws', region='Region01',
#' countOnly=TRUE)
#'
#' df <- sc_get_nlcd(year='2001', aoi='ws', region='Region01',
#' showAreaSqKm=FALSE, showPctFull=TRUE)
#'
#' df <- sc_get_nlcd(year='2001, 2006', aoi='cat,ws',
#' comid='179,1337,1337420')
#' }
#' @export

sc_get_nlcd <- function(year = '2019',
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

#' @rdname sc_get_nlcd
#' @keywords internal
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
  lifecycle::deprecate_warn("0.10.0", "sc_nlcd()", "sc_get_nlcd()")
  sc_get_nlcd(year = '2019',
              comid = NULL,
              aoi = NULL,
              showAreaSqKm = NULL,
              showPctFull = NULL,
              state = NULL,
              county = NULL,
              region = NULL,
              conus = NULL,
              countOnly = NULL)
}

#' @title Get NNI
#' 
#' @description 
#' Function to get all NNI data available for a given year.
#' 
#' @author
#' Selia Markley
#'
#' @param year Years(s) of NNI metrics to query.
#' Only valid NNI years are accepted (1987:2017)
#' Syntax: year=<year1>,<year2>
#' 
#' @param aoi Specify the area of interest described by a metric. By default, all available areas of interest
#' for a given metric are returned.
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: catchment|watershed
#' 
#' @param comid Return metric information for specific COMIDs
#' Syntax: comid=<comid1>,<comid2>
#' 
##' @param state Return metric information for COMIDs within a specific state. Use a state's abbreviation to
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
#' The default value is true.
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
#' \dontrun{
#' 
#' df <- sc_get_nni(year='1987, 1990, 2005, 2017', aoi='cat,ws',
#' comid='179,1337,1337420')
#' 
#' df <- sc_get_nni(year='2015', aoi='cat',
#' comid='179', countOnly=TRUE)
#' 
#' df <- sc_get_nni(comid='179', year='2011, 2012', aoi='ws')
#' 
#' df <- sc_get_nni(year='2015, 2016, 2017', county='41003', aoi='ws')
#' }
#' @export

sc_get_nni <- function(year, aoi = NULL, comid = NULL,
                      showAreaSqKm = TRUE, state = NULL,
                      county = NULL, region = NULL,conus = NULL, 
                      showPctFull = NULL,countOnly = NULL) {
  # year must be a character string.
  year_chr <-  as.character(year)
  # split multiple years supplied as a single string into
  # a vector of years.
  year_vec <- unlist(strsplit(x = year_chr,
                              split = ",|, "))
  # Vector of valid NNI years to check inputs against.
  valid_years <- c('1987',
                   '1988',
                   '1989',
                   '1990',
                   '1991',
                   '1992',
                   '1993',
                   '1994',
                   '1995',
                   '1996',
                   '1997',
                   '1998',
                   '1999',
                   '2000',
                   '2001',
                   '2002',
                   '2003',
                   '2004',
                   '2005',
                   '2006',
                   '2007',
                   '2008',
                   '2009',
                   '2010',
                   '2011',
                   '2012',
                   '2013',
                   '2014',
                   '2015',
                   '2016',
                   '2017')
  # Stop early if any of the year(s) supplied are not found in the valid
  # years vec.
  stopifnot(
    "year must be a valid NNI year" = any(year_vec %in% valid_years)
  )
  # Vector of NNI metric names.
  nni <- c(
    'n_leg_',
    'n_ags_',
    'n_ff_',
    'n_uf_',
    'n_cf_',
    'n_cr_',
    'n_hw_',
    'n_lw_',
    'p_leg_',
    'p_ags_',
    'p_ff_',
    'p_uf_',
    'p_cr_',
    'p_hw_',
    'p_lw_'
  )
  # Add n_dep for available years
  ndep_year_vec <- year_vec[!year_vec %in% c('1987', '1988', '1989')]
  ndep_comb <- expand.grid('n_dep_', ndep_year_vec)
  ndep_mets <- paste0(ndep_comb$Var1,
                      ndep_comb$Var2,
                      collapse = ",",
                      recycle0 = TRUE)
  # Add p_dep for available years
  pdep_year_vec <- year_vec[!year_vec %in% c('1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996',
                                             '2014', '2015', '2016', '2017')]
  pdep_comb <- expand.grid('p_dep_', pdep_year_vec)
  pdep_mets <- paste0(pdep_comb$Var1,
                      pdep_comb$Var2,
                      collapse = ",",
                      recycle0 = TRUE)
  # Add n_usgsww and p_usgsww for available years
  ww_year_vec <- year_vec[year_vec %in% c('1988', '1990', '1992', '1996', '2000', '2004', '2008', '2012')]
  ww_comb <- expand.grid(c('p_usgsww_', 'n_usgsww_'), ww_year_vec)
  ww_mets <- paste0(ww_comb$Var1,
                    ww_comb$Var2,
                    collapse = ",",
                    recycle0 = TRUE)
  # Create a data frame of all NNI Metric and year combinations.
  all_comb <- expand.grid(nni, year_vec)
  # Concatenate the NLCD metric name with the supplied year(s) to create
  # valid metric names to submit to the API.
  nni_mets <- paste0(all_comb$Var1,
                     all_comb$Var2,
                     collapse = ",",
                     recycle0 = TRUE)
  # Combine all NNI metrics
  nni_mets_all <- paste0(nni_mets, ",", ndep_mets, ",", pdep_mets, ",", ww_mets)
  
  # Query the API.
  final_df <- sc_get_data(
    metric = nni_mets_all,
    aoi = aoi,
    comid = comid,
    state = state,
    county = county,
    showAreaSqKm = showAreaSqKm,
    showPctFull = showPctFull,
    conus = conus,
    countOnly = countOnly
  )
  # End of function. Return a data frame
  return(final_df)
}

#' @importFrom curl curl_fetch_memory
NULL


#' Validate COMIDs
#'
#' @description
#' Checks user supplied COMIDs against valid COMID values in StreamCat
#' and returns a message if any COMIDs are not valid COMIDs.
#' Called internally by
#' \code{\link{sc_get_data}} when \code{validate = TRUE}.
#'
#' @details
#' Validation works by querying the NLDI for valid COMIDs with the 
#' supplied COMIDs and comparing these COMIDs against those requested.
#' Any COMID absent from the official list is considered invalid — i.e., 
#' it does not correspond to an NHDPlusV2 catchment recognized by StreamCat.
#' Additionally, error is raised for any COMIDs that are part of the COMIDs 
#' connectors in the Great Lakes that are not included in StreamCat
#'
#' @param comids Integer or character vector of COMIDs to validate. Accepts
#'   the same formats as the \code{comid} parameter of \code{\link{sc_get_data}}:
#'   a numeric vector, a character vector, or a comma-separated string.
#'
#' @return A character vector of COMIDs from \code{comids} that were
#'   \emph{not} found in the StreamCat database. Returns \code{character(0)}
#'   if all supplied COMIDs are valid.
#'
#' @seealso \code{\link{sc_get_data}}
#'
#' @author Marc Weber
#'
#' @keywords internal

# Helper: normalize incoming IDs to a character vector of digits
.parse_comids <- function(x) {
  if (is.null(x)) return(character(0))
  
  # If it's a list, unlist it first
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  
  if (is.numeric(x)) {
    return(as.character(as.integer(x)))
  }
  
  if (is.character(x)) {
    if (length(x) == 1L) {
      # Split by commas/whitespace; strip non-digits (e.g., quotes, parentheses)
      parts <- unlist(strsplit(x, "[,\\s]+", perl = TRUE), use.names = FALSE)
      parts <- gsub("[^0-9]", "", parts, perl = TRUE)
      parts <- parts[nzchar(parts)]
      return(parts)
    } else {
      parts <- trimws(x)
      parts <- gsub("[^0-9]", "", parts, perl = TRUE)
      parts <- parts[nzchar(parts)]
      return(parts)
    }
  }
  
  stop("Unsupported id input type. Provide numeric, character, vector, list, or a comma-separated string.")
}

# Assert-style validator
# - online = TRUE (default): query NLDI (nhdplusTools::get_nldi_feature) to confirm existence.
# - online = FALSE: validate against a local .valid_comids vector (must be available).
# - Also errors if an ID is present (valid) but listed in .missing_COMIDs/.missing_comids.
is_valid_comid <- function(comid, online = TRUE) {
  ids_chr <- .parse_comids(comid)
  if (!length(ids_chr)) return(invisible(ids_chr))
  
  # Convert to integer where possible
  ids_int <- suppressWarnings(as.integer(ids_chr))
  not_numeric <- is.na(ids_int)
  
  # Gather expected-missing list (accept either spelling)
  expected_missing <- integer(0)
  if (exists(".missing_COMIDs", inherits = TRUE)) {
    expected_missing <- c(expected_missing, get(".missing_COMIDs", inherits = TRUE))
  }
  if (exists(".missing_comids", inherits = TRUE)) {
    expected_missing <- c(expected_missing, get(".missing_comids", inherits = TRUE))
  }
  expected_missing <- unique(suppressWarnings(as.integer(expected_missing)))
  
  if (isTRUE(online)) {
    if (!requireNamespace("nhdplusTools", quietly = TRUE)) {
      stop("Online validation requires nhdplusTools. Install it or call assert_valid_comids(..., online = FALSE).", call. = FALSE)
    }
    
    # Query NLDI only for unique, numeric-looking IDs
    query_ids <- unique(ids_chr[!not_numeric])
    exists_map <- setNames(logical(length(query_ids)), query_ids)
    
    exists_map[] <- vapply(
      query_ids,
      function(i) {
        out <- tryCatch(
          nhdplusTools::get_nldi_feature(list(featureSource = "comid", featureID = i)),
          error = function(e) NULL
        )
        isTRUE(inherits(out, "sf")) && nrow(out) > 0
      },
      logical(1)
    )
    
    found <- rep(FALSE, length(ids_chr))
    if (length(exists_map)) {
      found_vals <- exists_map[match(ids_chr, names(exists_map))]
      found_vals[is.na(found_vals)] <- FALSE
      found[!not_numeric] <- found_vals[!not_numeric]
    }
    
  } else {
    # Offline validation requires .valid_comids
    if (!exists(".valid_comids", inherits = TRUE)) {
      stop("Local validation requested but .valid_comids not found. Provide it or set online = TRUE.", call. = FALSE)
    }
    valid_local <- (!not_numeric) & (ids_int %in% .valid_comids)
    found <- valid_local
  }
  
  # Compute error conditions
  not_found <- (!not_numeric) & (!found)
  flagged_present <- found & (!is.na(ids_int)) & (ids_int %in% expected_missing)
  
  if (any(not_numeric | not_found | flagged_present)) {
    msg <- character(0)
    if (any(not_numeric)) {
      msg <- c(msg, sprintf("Non-numeric COMID(s): %s", paste(unique(ids_chr[not_numeric]), collapse = ", ")))
    }
    if (any(not_found)) {
      src <- if (isTRUE(online)) "NLDI" else ".valid_comids"
      msg <- c(msg, sprintf("Not found in %s: %s", src, paste(unique(ids_chr[not_found]), collapse = ", ")))
    }
    if (any(flagged_present)) {
      which_missing <- unique(ids_chr[flagged_present])
      src <- if (exists(".missing_COMIDs", inherits = TRUE)) ".missing_COMIDs" else ".missing_comids"
      msg <- c(msg, sprintf("Valid COMID but one of Great Lake connector or other COMIDs not included in StreamCat in %s: %s", src, paste(which_missing, collapse = ", ")))
    }
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }
  
  # All good; return invisibly
  invisible(ids_int)
}

.missing_comids <- as.integer(c(
  904020528, 904020529, 904020533, 12211194, 12211232, 12211228, 12211188,
  12211202, 12211208, 12211224, 25823896, 12211204, 12211196, 12211192,
  25823892, 12211212, 12211186, 12211200, 12211206, 12211216, 12211220,
  12211174, 13196000, 13196024, 13196020, 13196028, 13196016, 13189762,
  13196010, 13189316, 13189392, 13189520, 13190016, 13189920, 13189524,
  13189564, 13189118, 13189156, 13189960, 13189370, 13189530, 13189356,
  13189598, 13189244, 13189596, 13189584, 13189754, 13189758, 13189378,
  13189692, 13189768, 13190242, 13189680, 13189602, 13189830, 13189306,
  13189304, 13189114, 13189366, 13189308, 13189866, 13189694, 13189836,
  13189710, 13189776, 13189712, 13189572, 13189576, 13189714, 13189764,
  13189398, 13189938, 13189224, 13189808, 13189664, 13189566, 13189842,
  13189590, 13189718, 13189582, 13189124, 13189756, 13189958, 13189388,
  13189580, 13189736, 13189672, 13196034, 13189748, 13189906, 13189676,
  13189540, 13189568, 13189868, 13189846, 13189066, 13189720, 13189860,
  13189926, 13189546, 13189804, 13191228, 13189822, 13189914, 13189740,
  904090018, 13189662, 13189744, 13190240, 13189814, 13189750, 13189834,
  13189158, 13189082, 13190024, 13189294, 13189820, 13191224, 13189844,
  13189852, 13189936, 13189984, 13189870, 13189794, 13189706, 13189818,
  13189594, 13189062, 13189588, 13189064, 13189974, 13189704, 13191214,
  13196004, 13189534, 13189950, 13189674, 13189942, 13189682, 13189922,
  13189102, 13189116, 13189928, 13189956, 13189386, 13189072, 13189792,
  13189660, 13189972, 13191220, 13191192, 13189666, 13189954, 13189526,
  13189826, 13190238, 13189932, 13189934, 13190022, 13189390, 904090019,
  13189068, 13189952, 13191204, 13189542, 13190018, 10850256, 13189872,
  13189790, 13189986, 13191208, 13189862, 13189586, 13189708, 13189552,
  13189668, 13189864, 13189536, 13189962, 13189080, 13189904, 13191230,
  13189832, 13189948, 13189746, 13189910, 13189678, 13189670, 13191232,
  13189732, 13189854, 13189912, 13189242, 10850230, 13189812, 13189522,
  13191222, 13189916, 13189728, 13191226, 13189702, 13189592, 13189802,
  13189700, 13190020, 13189726, 13189908, 13189946, 13189848, 13189528,
  13189778, 13189774, 13189810, 13191106, 13189698, 13189532, 10850250,
  13189930, 10850252, 13191206, 13191216, 13189296, 13189742, 13189850,
  13191218, 10850240, 10850244, 10850010, 13189918, 10849982, 13189976,
  10850242, 10850268, 13189924, 10850282, 10850264, 10850054, 10850232,
  10850280, 10850288, 10850258, 10850348, 10850320, 13191138, 10850006,
  10850052, 10850022, 10850394, 10850334, 10850284, 10850314, 10850030,
  10850340, 166764012, 10848664, 10848690, 10850266, 10849992, 10850290,
  10851378, 166764013, 10850286, 10850040, 10850018, 10848684, 10848666,
  10850044, 10850350, 10850274, 10850344, 10850272, 10850326, 10848688,
  26917654, 10850342, 10849990, 10850058, 10850328, 10850262, 10850032,
  10850278, 10850014, 10850338, 10848680, 10850332, 10851384, 10850316,
  10850322, 10850312, 10850310, 10850036, 10850050, 10850392, 10850228,
  166764011, 10850038, 10850292, 10850330, 26917656, 10851382, 10850346,
  15569793, 15568799, 15568857, 15568125, 15568855, 15567833, 15571659,
  15568091, 15568149, 15569901, 15568009, 15568783, 15568131, 15568003,
  15569797, 15568093, 15568129, 15568011, 15567983, 15568143, 15568107,
  15568113, 15568109, 15569803, 166764152, 15571665, 904120018, 15568121,
  15567993, 15568015, 15569799, 15569907, 15568135, 15571657, 15567989,
  15568119, 15569809, 15569791, 166764154, 904120017, 15569903, 15568147,
  30833944, 15568141, 904120016, 15568127, 166764151, 15568007, 15568139,
  15567987, 15569905, 15567997, 15568145, 15502697, 25293394, 15502723,
  25293246, 25293298, 25293206, 25293274, 25293362, 25293232, 15502711,
  25293390, 25293180, 15502719, 25293330, 15502715, 25293332, 25293184,
  25293366, 25293350, 25293186, 25293398, 25293270, 25293324, 25293410,
  25293322, 25293294, 15502727, 15502695, 25293386, 25293208, 25293234,
  25293230, 25293364, 25293272, 25293406, 25293402, 25293196, 25293202,
  25293306, 25293222, 25293296, 25293236, 25293188
))

