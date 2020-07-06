#' @title Get all possible values of a parameter
#' @description All possible values of a parameter for a given query. Helps to
#'   understand the columns of data.frame from \code{\link{get_streamcat_data}}.
#' @param param A valid parameter value. Available names are: areaOfInterest,
#'   comid, countOnly, county, name, region,
#'   showAreaSqKm, showPctFull, state.
#' @return Character vector of all possible parameter values.
#' @export get_streamcat_params
#' @examples
#'
#' \dontrun{
#' # Return list of available regions for extracting data
#' nass_param("state")
#' }
#'
#' \dontrun{
#' # Return list of all possible StreamCat metrics
#' nass_param("name")
#' }

# get_url <- "http://v26267mcpk506/StreamCat/v1/stable/metrics"
# resp <- httr::GET(get_url)
# df <- httr::content(resp, type="text/csv", encoding = 'UTF-8') 
# df <- df[,1:ncol(df)-1]
# 
# nass_param <- function(param = NULL,
#                        source_desc = NULL,
#                        sector_desc = NULL,
#                        group_desc = NULL,
#                        commodity_desc = NULL,
#                        class_desc = NULL,
#                        prodn_practice_desc = NULL,
#                        util_practice_desc = NULL,
#                        statisticcat_desc = NULL,
#                        unit_desc = NULL,
#                        short_desc = NULL,
#                        domain_desc = NULL,
#                        domaincat_desc = NULL,
#                        agg_level_desc = NULL,
#                        state_ansi = NULL,
#                        state_fips_code = NULL,
#                        state_alpha = NULL,
#                        state_name = NULL,
#                        asd_code = NULL,
#                        asd_desc = NULL,
#                        county_ansi = NULL,
#                        county_code = NULL,
#                        county_name = NULL,
#                        region_desc = NULL,
#                        zip_5 = NULL,
#                        watershed_code = NULL,
#                        watershed_desc = NULL,
#                        congr_district_code = NULL,
#                        country_code = NULL,
#                        country_name = NULL,
#                        location_desc = NULL,
#                        year = NULL,
#                        freq_desc = NULL,
#                        begin_code = NULL,
#                        end_code = NULL,
#                        reference_period_desc = NULL,
#                        week_ending = NULL,
#                        key = NULL){
#   
#   # Pass the arguments through formatting
#   calls      <- match.call(expand.dots = TRUE)
#   calls[[1]] <- as.name("args_list")
#   arguments  <- eval.parent(calls)
#   
#   
#   base_url <- paste0("http://quickstats.nass.usda.gov/api/get_param_values/")
#   temp_url <- httr::modify_url(base_url, query = arguments)
#   
#   if (!is.null(param)) {
#     temp     <- httr::GET(temp_url)
#     tt       <- check_response(temp)
#     
#     if (names(tt) == param) {
#       param_data <- as.character(unlist(tt))
#     } else {
#       stop("Parameter entered is not valid")
#     }
#   }  else{
#     stop("Please enter a parameter category")
#   }
#   
#   return(param_data)
# }
