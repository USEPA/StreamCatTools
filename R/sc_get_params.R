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
  resp <- fromJSON("http://v26267mcpk506/StreamCat/v1/stable/metrics")
  if (param=='area') params <- resp$parameters$areaOfInterest$options else{
    params <- resp$parameters$name$options
  }
  return(params)
}
