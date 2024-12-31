#' @title Get StreamCat Parameters
#'
#' @description
#' Function to return available StreamCat parameters using the StreamCat API.
#'
#' @author
#' Marc Weber
#'
#' @param param List of available parameters in the API for the following options:
#' name, areaofInterest, region, state, county
#' Syntax: param=<value1>,<value2>
#' Values: name|area
#'
#' @return A list of all the current StreamCat values for a given parameter
#' @export
#'
#' @examples
#' params <- sc_get_params(param='name')
#' params <- sc_get_params(param='areaOfInterest')

sc_get_params <- function(param = NULL) {
  resp <- jsonlite::fromJSON("https://api.epa.gov/StreamCat/streams/metrics")$items
  if (param=='areaOfInterest'){
    params <- strsplit(stringr::str_sub(resp$aoi_param_info[[1]]$options,2,-2),",")[[1]]
    params <- gsub(" ","", params)
  }  else {
    params <- resp$name_options[[1]][[1]]
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
