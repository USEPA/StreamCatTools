#' @title Get StreamCat Parameters
#'
#' @description
#' Function to return available StreamCat parameters using the StreamCat API.
#'
#' @author
#' Marc Weber
#'
#' @param param List of available parameters in the API for the following options:
#' name, areaofInterest, region, state, state_name, state_fips, county, county_fips
#' Syntax: param=<value1>,<value2>
#' Values: name|area
#'
#' @return A list of all the current StreamCat values for a given parameter
#' @export
#'
#' @examples
#' params <- sc_get_params(param='name')
#' params <- sc_get_params(param='areaOfInterest')
#' params <- sc_get_params(param='state')
#' params <- sc_get_params(param='state_name')
#' params <- sc_get_params(param='state_fips')
#' params <- sc_get_params(param='county')
#' params <- sc_get_params(param='county_fips')

sc_get_params <- function(param = NULL) {
  resp <- jsonlite::fromJSON("https://api.epa.gov/StreamCat/streams/metrics")$items
  if (param=='areaOfInterest'){
    params <- strsplit(stringr::str_sub(resp$aoi_param_info[[1]]$options,2,-2),",")[[1]]
    params <- c(gsub(" ","", params),'other')
  }  else if(param == 'name') {
    params <- resp$name_options[[1]][[1]]
  } else if(param == 'region'){
    params <- resp$region_options[[1]][[1]]
  } else if(param == 'state'){
    params <- resp$state_options[[1]]$st_abbr
    params <- params[!params %in% c('AK','HI','PR')]
  } else if(param == 'state_name'){
    params <- resp$state_options[[1]]$st_name
    params <- params[!params %in% c('Alaska','Hawaii','Puerto Rico')]
  } else if(param == 'state_fips'){
    params <- as.character(resp$state_options[[1]]$st_fips)
    params[nchar(params) < 2] <- paste0('0',params[nchar(params) < 2])
    params <- params[!params %in% c('02','15','72')]
  } else if(param == 'county'){
    params <- paste0(resp$county_options[[1]]$county_name, ' ',resp$county_options[[1]]$state)
  } else if(param == 'county_fips'){
    params <- as.character(resp$county_options[[1]]$fips)
    params[nchar(params) < 5] <- paste0('0',params[nchar(params) < 5])
  }
  params <- params[order(params)]
  return(params)
}

#' @title Lookup Full Metric Name
#'
#' @description
#' Function to retrieve a full metric name based on the short name using the StreamCat API.
#'
#' @author
#' Marc Weber
#'
#' @param metric Short metric name
#' Syntax: metric=value1
#' Values: metric
#'
#' @return A lookup of the full name for a given StreamCat metric
#' @export
#'
#' @examples
#' fullname <- sc_fullname(metric='clay')

sc_fullname <- function(metric = NULL) {
  resp <- jsonlite::fromJSON("https://api.epa.gov/StreamCat/streams/datadictionary")$items
  resp <- as.data.frame(resp$dictionary)
  result <- unique(resp[resp$metric_prefix %in% unlist(strsplit(metric, split = ',')), 1])
  return(result)
}
