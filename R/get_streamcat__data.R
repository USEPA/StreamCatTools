
library(httr)
libary(readr)
library(dplyr)
library(tidyselect)
get_streamcat_data <- function(metric='fert', aoi='catchment', comid='179') {
  base_url <- "http://v26267mcpk506/StreamCat/v1/stable/metrics"
  url <- paste0(base_url, "?name=",metric,"&areaOfInterest=",aoi,"&comid=",comid)
  resp <- httr::GET(url)
  df <- content(resp, type="text/csv") 
  df <- df[,1:ncol(df)-1] 
  return(df)
}

df <- get_streamcat_data()
resp <- streamcat_api("http://v26267mcpk506/StreamCat/v1/stable/metrics")
resp

library(curl)
req <- curl_fetch_memory("http://v26267mcpk506/StreamCat/v1/stable/metrics?name=fert&areaOfInterest=cat&comid=179")
str(req)

tmp <- tempfile()
curl_download("http://204.47.146.253:8080/StreamCat/catinfo?pmetric=SuperfundDens&pAreaOfInterest=watershed&pregion=Region01&prequestType=region",tmp)
library(readr)
test <- read_csv(req$content)
