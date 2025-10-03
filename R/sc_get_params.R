#' @title Get StreamCat Parameters
#'
#' @description
#' Function to return available StreamCat parameters using the StreamCat API.
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
#' @return A list of all the current StreamCat values for a given parameter
#' @export
#'
#' @examples
#' \dontrun{
#' params <- sc_get_params(param='variable_info')
#' params <- sc_get_params(param='metric_names')
#' params <- sc_get_params(param='categories')
#' params <- sc_get_params(param='aoi')
#' params <- sc_get_params(param='state')
#' params <- sc_get_params(param='county')
#' params <- sc_get_params(param='datasets')}

sc_get_params <- function(param = NULL) {
  UUID <- DATE_DOWNLOADED <- METADATA <- FINAL_TABLE<- NULL
  INDICATOR_CATEGORY <- METRIC_NAME <- AOI <- YEAR <- NULL
  WEBTOOL_NAME <- METRIC_UNITS <- METRIC_DESCRIPTION <- DSID <- NULL
  SOURCE_NAME <- SOURCE_URL <- UUID <- DATE_DOWNLOADED <- NULL
  resp <- jsonlite::fromJSON("https://api.epa.gov/StreamCat/streams/metrics")$items
  if (param=='aoi'){
    params <- strsplit(stringr::str_sub(resp$aoi_param_info[[1]]$options,2,-2),",")[[1]]
    params <- c(gsub(" ","", params),'other')
    params <- params[order(params)]
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
  } else if(param == 'categories'){
    params <- httr2::request('https://api.epa.gov/StreamCat/streams/variable_info') |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      readr::read_csv(show_col_types = FALSE) |> 
      dplyr::select(INDICATOR_CATEGORY)
    params <- sort(unique(params$INDICATOR_CATEGORY))
  } else if(param == 'datasets'){
    params <- httr2::request('https://api.epa.gov/StreamCat/streams/variable_info') |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      readr::read_csv(show_col_types = FALSE) |> 
      dplyr::select(DSNAME)
    params <- sort(unique(params$DSNAME[!is.na(params$DSNAME)]))  
  } 
  else if(param == 'region'){
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
  result <- unique(resp$short_display_name[resp$metric_prefix %in% metric])
  return(result)
}


#' Get StreamCat Metric Names
#'
#' @description
#' Function to filter StreamCat metrics metrics by category, area of interest, 
#' dataset or year. Use `sc_get_params(categories)` or `sc_get_params(datasets)`
#' to see all the valid category or dataset options
#'
#' @author
#' Marc Weber
#' 
#' @param category Filter StreamCat metrics based on the metric category
#' @param aoi Filter StreamCat metrics based on the area of interest
#' @param year Filter StreamCat metrics based on a particular year or years
#' @param dataset Filter StreamCat metrics based on the dataset name
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' metrics <- sc_get_metric_names(category='Wildfire')
#' 
#' metrics <- sc_get_metric_names(category = c('Deposition','Climate'),
#' aoi=c('Cat','Ws')
#' metrics <- sc_get_metric_names(aoi='Other',
#' dataset=c('Canal Density','Predicted Channel Widths Depths'))
#' 
#' } 


sc_get_metric_names <- function(category = NULL,
                                aoi = NULL,
                                year = NULL,
                                dataset = NULL) {
  if (!is.null(aoi)){
    if (any(stringr::str_detect(aoi,'catchment'))) {
      aoi <- gsub('catchment','Cat',aoi)
    }
    if (any(stringr::str_detect(aoi,'watershed'))) {
      aoi <- gsub('watershed','Ws',aoi)
    }
    if (any(stringr::str_detect(aoi,'riparian_catchment'))) {
      aoi <- gsub('riparian_catchment','CatRp100',aoi)
    }
    if (any(stringr::str_detect(aoi,'riparian_watershed'))) {
      aoi <- gsub('riparian_watershed','WsRp100',aoi)
    }
    aoi <- stringr::str_to_title(aoi)
  }
  resp <- params <- httr2::request('https://api.epa.gov/StreamCat/streams/variable_info') |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    readr::read_csv(show_col_types = FALSE)
  
  filters <- list(INDICATOR_CATEGORY = category, AOI = aoi, YEAR = year,
                  DSNAME = dataset)
  
  filter_data <- function(data, filters) {
    # Filter the data frame for each non-null filter
    filtered_data <- purrr::reduce(
      names(filters),
      .init = data,
      .f = function(df, col_name) {
        filter_values <- filters[[col_name]]
        if (!is.null(filter_values)) {
          df <- df %>%
            mutate(temp_col = str_split(.data[[col_name]], ",")) %>%
            filter(purrr::map_lgl(temp_col, ~ any(.x %in% filter_values))) %>%
            select(-temp_col)
        }
        df
      }
    )
    return(filtered_data)
  }
  results <- filter_data(resp, filters)
  results <- results |> 
    dplyr::select(Category = INDICATOR_CATEGORY, Metric = METRIC_NAME, 
                           AOI,Year = YEAR, Short_Name = WEBTOOL_NAME,
                           Metric_Description = METRIC_DESCRIPTION, 
                           Units = METRIC_UNITS, Source = SOURCE_NAME, 
                           Dataset = DSNAME)
      
  return(results)
}
