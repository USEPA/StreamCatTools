#' @title Get LakeCat Parameters
#'
#' @description
#' Function to return available LakeCat parameters using the StreamCat API.
#'
#' @author
#' Marc Weber
#'
#' @param param List of available parameters in the API for the following options:
#' name, areaofInterest, region, state, county.  State and county return a data
#' frame that includes FIPS codes, names and state abbreviations 
#' Syntax: param=<value1>,<value2>
#' Values: name|area
#'
#' @return A list of all the current LakeCat values for a given parameter
#' @export
#'
#' @examples
#' \donttest{
#' params <- lc_get_params(param='variable_info')
#' params <- lc_get_params(param='metric_names')
#' params <- lc_get_params(param='areaOfInterest')
#' params <- lc_get_params(param='state')
#' params <- lc_get_params(param='county')
#' }

lc_get_params <- function(param = NULL) {
  UUID <- DATE_DOWNLOADED <- METADATA <- FINAL_TABLE<- NULL
  INDICATOR_CATEGORY <- METRIC_NAME <- AOI <- YEAR <- NULL
  WEBTOOL_NAME <- METRIC_UNITS <- METRIC_DESCRIPTION <- DSID <- NULL
  SOURCE_NAME <- SOURCE_URL <- UUID <- DATE_DOWNLOADED <- NULL
  resp <- jsonlite::fromJSON("https://api.epa.gov/StreamCat/lakes/metrics")$items
  if (param=='areaOfInterest'){
    params <- strsplit(stringr::str_sub(resp$aoi_param_info[[1]]$options,2,-2),",")[[1]]
    params <- c(gsub(" ","", params),'other')
    params <- params[order(params)]
    params <- params[!params %in% c('catrp100','wsrp100','other')]
  }  else if(param == 'metric_names') {
    params <- resp$name_options[[1]][[1]]
    params <- params[!duplicated(params)]
    params <- params[order(params)]
  } else if(param == 'variable_info') {
    params <- httr2::request('https://api.epa.gov/StreamCat/streams/variable_info') |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      readr::read_csv(show_col_types = FALSE) |> 
      dplyr::select(-UUID,-DATE_DOWNLOADED,-METADATA) |> 
      dplyr::rename(dataset=FINAL_TABLE,category=INDICATOR_CATEGORY, 
                    metric=METRIC_NAME,aoi=AOI, year=YEAR, 
                    short_description=WEBTOOL_NAME,units=METRIC_UNITS,
                    long_description=METRIC_DESCRIPTION, dsid=DSID,
                    source_name=SOURCE_NAME, source_URL=SOURCE_URL)
  } else if(param == 'region'){
    params <- resp$region_options[[1]][[1]]
    params <- params[order(params)]
  } else if(param == 'state'){
    params <- resp$state_options[[1]]
    params <- params[!params$st_abbr %in% c('AK','HI','PR'),]
    params$st_fips <- as.character(params$st_fips)
    params$st_fips[nchar(params$st_fips) < 2] <- paste0('0',params$st_fips[nchar(params$st_fips) < 2])
    params <- params[order(params$st_name),]
    rownames(params) <- 1:nrow(params)
  } else if(param == 'county'){
    params <- resp$county_options[[1]]
    params$fips <- as.character(params$fips)
    params$fips[nchar(params$fips) < 5] <- paste0('0',params$fips[nchar(params$fips) < 5])
    params <- params[with(params,order(state,county_name)),]
    rownames(params) <- 1:nrow(params)
  }
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
  result <- unique(resp$short_display_name[resp$metric_prefix==metric])
  return(result)
}
