#' @title Get StreamCat Metrics
#' 
#' @description 
#' Function to return available StreamCat metrics using the StreamCat API.  The function allows a user to get data
#' 
#' @author 
#' Marc Weber
#' 
#' @param url The hard-wired url to grab JSON of parameters in API
#' 
#' @return A list of all the current StreamCat metrics
#' @export
#'
#' @examples
#' metrics <- get_streamcat_metrics()

get_streamcat_metrics <- function(url = "http://v26267mcpk506/StreamCat/v1/stable/metrics") {
  resp <- fromJSON(url)
  metrics <- resp$parameters$name$options
  return(metrics)
}
