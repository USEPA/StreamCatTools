#' @title Get StreamCat Parameters
#' 
#' @description 
#' Function to return available StreamCat parameters using the StreamCat API.  
#' 
#' @author 
#' Marc Weber
#' 
#' @param param Either name or area to grab JSON of parameters in API
#' Syntax: param=<value1>,<value2>
#' Values: name|area
#'  
#' @return A list of all the current StreamCat values for a given parameter
#' @export
#'
#' @examples
#' params <- get_streamcat_params(param='name')
#' params <- get_streamcat_params(param='area')

sc_get_params <- function(param = NULL) {
  resp <- jsonlite::fromJSON("https://v26267mcpk506/StreamCat/v1/stable/metrics")
  if (param=='areaOfInterest') params <- resp$parameters$areaOfInterest$options else{
    params <- resp$parameters$name$options
  }
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
#' @param metric 
#' Syntax: metric=value1
#' Values: metric
#'  
#' @return A lookup of the full name for a given StreamCat metric
#' @export
#'
#' @examples
#' fullname <- sc_fullname(metric='name')

sc_fullname <- function(metric = NULL) {
  resp <- fromJSON("https://v26267mcpk506/StreamCat/v1/stable/metrics/datadictionary")
  result <- resp$dictionary[[metric]]$short_display_name
  return(result)
}
