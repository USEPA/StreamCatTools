#' @title Get StreamCat data
#' 
#' @description 
#' Function to return StreamCat catchment and watershed metrics using the StreamCat API.  The function allows a user to get
#' specific metric data aggregated by area of interest, returned by comid(s), hydroregion(s), state(s), or county(ies).
#' 
#' @author 
#' Marc Weber
#' 
#' @param metric Name(s) of metrics to query
#' Syntax: name=<name1>,<name2>
#' 
#' @param aoi Specify the area of interest described by a metric. By default, all available areas of interest 
#' for a given metric are returned.
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: catchment|watershed|riparian_catchment|riparian_watershed|other
#' 
#' @param comid Return metric information for specific COMIDs.  Needs to be a character string
#' and function will convert to this format if needed.
#' Syntax: comid=<comid1>,<comid2>
#' 
#' @param state Return metric information for COMIDs within a specific state. Use a state's abbreviation to 
#' query for a given state.
#' Syntax: state=<state1>,<state2>
#' 
#' @param county Return metric information for COMIDs within a specific county. 
#' Users must use the FIPS code, not county name, as a way to disambiguate counties.
#' Syntax: county=<county1>,<county1>
#' 
#' @param region Return metric information for COMIDs within a specified hydroregion.
#' Syntax: region=<regionid1>,<regionid2>
#'
#' @param conus Return all COMIDs in the conterminous United States. 
#' The default value is false.
#' Values: true|false
#'  
#' @param showAreaSqKm Return the area in square kilometers of a given area of interest. 
#' The default value is false.
#' Values: true|false 
#' 
#' @param showPctFull Return the pctfull for each dataset. The default value is false.
#' Values: true|false
#' 
#' @param countOnly Return a CSV containing only the row count (ROWCOUNT) and the column 
#' count (COLUMNCOUNT) that the server expects to return in a request. The default value is false.
#' Values: true|false
#' 
#' @return A tibble of desired StreamCat metrics
#'
#' @examples
#' \donttest{
#' df <- sc_get_data(comid='179', aoi='catchment', metric='fert')
#' 
#' df <- sc_get_data(metric='PctGrs2006', aoi='watershed', region='01')
#' 
#' df <- sc_get_data(metric='PctUrbMd2006', aoi='riparian_catchment', 
#' comid='1337420')
#' 
#' df <- sc_get_data(metric='PctUrbMd2006,DamDens', 
#' aoi='catchment,watershed', comid='179,1337,1337420')
#'  }
#' @export

sc_get_data <- function(metric=NA, aoi=NA, comid=NA, state=NA, county=NA, 
                        region=NA, showAreaSqKm=NA, showPctFull=NA, conus=NA,
                        countOnly=NA) {
  query_url <- "https://java.epa.gov/StreamCAT/metrics?"
  if (any((!is.character(comid) & ! is.na(comid)) | (length(comid > 1) &  ! is.na(comid)))){
    comid <- paste(comid, collapse=",")
    if (stringr::str_detect(comid,'NA')) comid <- str_remove(comid,',NA')
  }
  post_body=""
  if (!is.na(metric)) post_body <- paste0(post_body,"&name=",metric)
  if (!is.na(comid)) post_body <- paste0(post_body,"&comid=",comid) 
  if (!is.na(aoi)) post_body <- paste0(post_body,"&areaOfInterest=",aoi) 
  if (!is.na(state)) post_body <- paste0(post_body,"&state=",state) 
  if (!is.na(county)) post_body <- paste0(post_body,"&county=",county) 
  if (!is.na(region)) post_body <- paste0(post_body,"&region=",region) 
  if (!is.na(showAreaSqKm)) post_body <- paste0(post_body,"&showAreaSqKm=",showAreaSqKm)
  if (!is.na(showPctFull)) post_body <- paste0(post_body,"&showPctFull=",showPctFull) 
  if (!is.na(conus)) post_body <- paste0(post_body,"&conus=",conus)
  if (!is.na(countOnly)) post_body <- paste0(post_body,"&countOnly=",countOnly)
  post_body = substring(post_body, 2)
  resp <- httr::POST(query_url, body=post_body)
  df <- httr::content(resp, type="text/csv", encoding = 'UTF-8',show_col_types = FALSE) 
  return(df)
}

#' @title Get NLCD Data
#' 
#' @description 
#' Function to specifically retrieve all NLCD metrics for a given year using the StreamCat API.  
#' 
#' @author 
#' Marc Weber
#' 
#' @param year Years(s) of NLCD metrics to query.
#' Only valid NLCD years are accepted (i.e. 2001, 2004, 2006, 2008, 
#' 2011, 2013, 2016, 2019)
#' Syntax: year=<year1>,<year2>
#' 
#' @param aoi Specify the area of interest described by a metric. By default, all available areas of interest 
#' for a given metric are returned.
#' Syntax: areaOfInterest=<value1>,<value2>
#' Values: catchment|watershed|riparian_catchment|riparian_watershed|other
#' 
#' @param comid Return metric information for specific COMIDs
#' Syntax: comid=<comid1>,<comid2>
#' 
#' @param state Return metric information for COMIDs within a specific state. Use a state's abbreviation to 
#' query for a given state.
#' Syntax: state=<state1>,<state2>
#' 
#' @param county Return metric information for COMIDs within a specific county. 
#' Users must use the FIPS code, not county name, as a way to disambiguate counties.
#' Syntax: county=<county1>,<county1>
#' 
#' @param region Return metric information for COMIDs within a specified hydroregion.
#' Syntax: region=<regionid1>,<regionid2>
#'
#' @param conus Return all COMIDs in the conterminous United States. 
#' The default value is false.
#' Values: true|false
#'  
#' @param showAreaSqKm Return the area in square kilometers of a given area of interest. 
#' The default value is false.
#' Values: true|false 
#' 
#' @param showPctFull Return the pctfull for each dataset. The default value is false.
#' Values: true|false
#' 
#' @param countOnly Return a CSV containing only the row count (ROWCOUNT) and the column 
#' count (COLUMNCOUNT) that the server expects to return in a request. The default value is false.
#' Values: true|false
#' 
#' @return A tibble of desired StreamCat metrics
#'
#' @examples
#' \donttest{
#' df <- sc_nlcd(year='2001', aoi='catchment',comid='179,1337,1337420')
#' 
#' df <- sc_nlcd(year='2001', aoi='watershed', region='01')
#' 
#' df <- sc_nlcd(year='2001, 2006', aoi='catchment,watershed', 
#' comid='179,1337,1337420')
#' }
#' @export 


sc_nlcd <- function(year='2019', aoi=NA, comid=NA, state=NA, county=NA, 
                    region=NA, showAreaSqKm=NA, showPctFull=NA, conus=NA,
                    countOnly=NA)  {
  nlcd <- c('PctMxFst','PctOw','PctShrb','PctUrbHi','PctUrbLo',
            'PctUrbMd','PctUrbOp','PctWdWet','PctBl','PctConif',
            'PctCrop','PctDecid','PctGrs','PctHay','PctHbWet',
            'PctIce')
  year=as.character(year)
  if (stringr::str_detect(year,',') & length(year)==1){
    year <- sapply(strsplit(year, ",")[[1]], trimws)
  }
  if (length(year)==1){
    if (year %in% c('2001', '2004', '2006', '2008', '2011', '2013',
                    '2016', '2019')){
      nlcd_mets <- paste0(nlcd, year, collapse = ",")
      df <- sc_get_data(metric=nlcd_mets, aoi=aoi, comid=comid, 
                        state=state, county=county, region=region, 
                        showAreaSqKm=showAreaSqKm, showPctFull=showPctFull, 
                        conus=conus,countOnly=countOnly)
      return(df)
      
    } else {
      stop("year must be a valid NLCD land cover year: 2001, 2004,
         2006, 2008, 2011, 2013, or 2019")
    }
  } else {
    k=1
    for (i in year){
      if (i %in% c('2001', '2004', '2006', '2008', '2011', '2013',
                      '2016', '2019')){
        nlcd_mets <- paste0(nlcd, i, collapse = ",")
        if (k==1){
          df <- sc_get_data(metric=nlcd_mets, aoi=aoi, comid=comid, 
                            state=state, county=county, region=region, 
                            showAreaSqKm=showAreaSqKm, showPctFull=showPctFull, 
                            conus=conus,countOnly=countOnly)
        } else {
          temp <- sc_get_data(metric=nlcd_mets, aoi=aoi, comid=comid, 
                            state=state, county=county, region=region, 
                            showAreaSqKm=showAreaSqKm, showPctFull=showPctFull, 
                            conus=conus,countOnly=countOnly)
          df <- cbind(df, temp[,4:19])
        }
        
      } else {
        stop("year must be a valid NLCD land cover year: 2001, 2004,
         2006, 2008, 2011, 2013, or 2019")
      }
      k=k+1
    }
    
  }
  return(df)
}