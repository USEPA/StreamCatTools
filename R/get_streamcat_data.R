#' @title Get StreamCat data
#' 
#' @description 
#' Function to return StreamCat catchment and watershed metrics using the StreamCat API.  The function allows a user to get data
#' specific to a catchment or metric of interest, as well as returning data by hydroregion(s), state(s), or county(ies).
#' 
#'
#' @param metric Names of a metric to query
#' Syntax: name=<name1>,<name2>
#' 
#' @param aoi Specify the area of interest described by a metric. By default, all available areas of interest 
#' for a given metric are returned.
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: catchment|watershed|riparian_catchment|riparian_watershed|other
#' 
#' @param comid Return metric information about specific COMIDs
#' Syntax: comid=<comid1>,<comid2>
#' 
#' @param state Return metric information for COMIDs within a specific state. Use a state's abbreviation to 
#' query for a given state.
#' Syntax: state=<state1>,<state2>
#' 
#' @param county Return metric information for COMIDs within a specific county. 
#' Users must use FIPS code, not county name, as a way to disambiguate counties.
#' Syntax: county=<county1>,<county1>
#' 
#' @param region Return metric information for COMIDs within a specified hydroregion.
#' Syntax: region=<regionid1>,<regionid2>
#' 
#' @param showPctFull Return the "*pctfull" row for each dataset. The default value is false.
#' Values: true|false 
#' 
#' @return A tibble of desired StreamCat metrics
#' @export
#'
#' @examples
#' df <- get_streamcat_data(comid='179', aoi='catchment', metric='fert')
#' 
#' df <- get_streamcat_data(metric='PctGrs2006', aoi='watershed', region='01')
#' 
#' df <- get_streamcat_data(metric='PctUrbMd2006', aoi='riparian_catchment', comid='1337420')
#' 
#' df <- get_streamcat_data(metric='PctUrbMd2006,DamDens,TRIDens', aoi='riparian_catchment,catchment,watershed', comid='179,1337,1337420')

get_streamcat_data <- function(metric=NA, aoi=NA, comid=NA, state=NA, county=NA, region=NA, showPctFull=NA) {
  get_url <- "http://v26267mcpk506/StreamCat/v1/stable/metrics?"
  if (!is.na(metric)) get_url <- paste0(get_url,"&name=",metric)
  if (!is.na(comid)) get_url <- paste0(get_url,"&comid=",comid) 
  if (!is.na(aoi)) get_url <- paste0(get_url,"&areaOfInterest=",aoi) 
  if (!is.na(state)) get_url <- paste0(get_url,"&state=",state) 
  if (!is.na(county)) get_url <- paste0(get_url,"&county=",county) 
  if (!is.na(region)) get_url <- paste0(get_url,"&region=",region) 
  if (!is.na(showPctFull)) get_url <- paste0(get_url,"&showPctFull=",showPctFull) 
  resp <- httr::GET(get_url)
  df <- httr::content(resp, type="text/csv", encoding = 'UTF-8') 
  df <- df[,1:ncol(df)-1] 
  return(df)
}

df <- get_streamcat_data(metric='PctUrbMd2006,DamDens,TRIDens', aoi='riparian_catchment,catchment,watershed', comid='179,1337,1337420')
