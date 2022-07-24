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
#' @export
#'
#' @examples
#' df <- sc_get_data(comid='179', aoi='catchment', metric='fert')
#' 
#' df <- sc_get_data(metric='PctGrs2006', aoi='watershed', region='01')
#' 
#' df <- sc_get_data(metric='PctUrbMd2006', aoi='riparian_catchment', comid='1337420')
#' 
#' df <- sc_get_data(metric='PctUrbMd2006,DamDens,TRIDens', aoi='riparian_catchment,catchment,watershed', comid='179,1337,1337420')

sc_get_data <- function(metric=NA, aoi=NA, comid=NA, state=NA, county=NA, 
                        region=NA, showAreaSqKm=NA, showPctFull=NA, conus=NA,
                        countOnly=NA) {
  query_url <- "https://v26267mcpk506/StreamCat/v1/stable/metrics?"
  if (!is.character(comid) & ! is.na(comid)) comid <- paste(comid, collapse=",")
  query_body=""
  if (!is.na(metric)) query_body <- paste0(query_body,"&name=",metric)
  if (!is.na(comid)) query_body <- paste0(query_body,"&comid=",comid) 
  if (!is.na(aoi)) query_body <- paste0(query_body,"&areaOfInterest=",aoi) 
  if (!is.na(state)) query_body <- paste0(query_body,"&state=",state) 
  if (!is.na(county)) query_body <- paste0(query_body,"&county=",county) 
  if (!is.na(region)) query_body <- paste0(query_body,"&region=",region) 
  if (!is.na(showAreaSqKm)) query_body <- paste0(query_body,"&showAreaSqKm=",showPctFull)
  if (!is.na(showPctFull)) query_body <- paste0(query_body,"&showPctFull=",showAreaSqKm) 
  if (!is.na(conus)) query_body <- paste0(query_body,"&conus=",conus)
  if (!is.na(countOnly)) query_body <- paste0(query_body,"&countOnly=",conus)
  resp <- httr::GET(paste0(query_url, body=query_body))
  df <- httr::content(resp, type="text/csv", encoding = 'UTF-8',show_col_types = FALSE) 
  return(df)
}
