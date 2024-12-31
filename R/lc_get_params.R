#' @title Get LakeCat Parameters
#'
#' @description
#' Function to return available LakeCat parameters using the StreamCat API.
#'
#' @author
#' Marc Weber
#'
#' @param param Either name or area to grab JSON of parameters in API
#' Syntax: param=<value1>,<value2>
#' Values: name|area
#'
#' @return A list of all the current LakeCat values for a given parameter
#' @export
#'
#' @examples
#' params <- lc_get_params(param='name')
#' params <- lc_get_params(param='areaOfInterest')

lc_get_params <- function(param = NULL) {
  resp <- jsonlite::fromJSON("https://api.epa.gov/StreamCat/lakes/metrics")$items
  if (param=='areaOfInterest') {
    params <- strsplit(stringr::str_sub(resp$aoi_param_info[[1]]$options,2,-2),",")[[1]]
    params <- gsub(" ","", params)[1:2]
  } else{
    params <- resp$name_options[[1]]$name
  }
  params <- params[order(params)]
  return(params)
}

#' @title Lookup Full Metric Name
#'
#' @description
#' Function to retrieve a full metric name based on the short name using the LakeCat API.
#'
#' @author
#' Marc Weber
#'
#' @param metric Short metric name
#' Syntax: metric=value1
#' Values: metric
#'
#' @return A lookup of the full name for a given LakeCat metric
#' @export
#'
#' @examples
#' fullname <- lc_fullname(metric='clay')

lc_fullname <- function(metric = NULL) {
  resp <- jsonlite::fromJSON("https://api.epa.gov/StreamCat/lakes/datadictionary")$items
  resp <- as.data.frame(resp$dictionary)
  result <- unique(resp[resp$metric_prefix %in% unlist(strsplit(metric, split = ',')), 1])
  return(result)
}
