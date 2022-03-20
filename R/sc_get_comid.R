#' @title Get COMIDs
#' 
#' @description 
#' Function to return NHDPlusV2 COMIDS using either a 
#' dataframe with coordinates and a specified CRS or an 
#' sf object.  The function generates a vector of COMID
#' values a user can then pass to sc_get_data function
#' 
#' @author 
#' Marc Weber
#' 
#' @param df Name of data frame object.  Can be a simple
#' data frame with coordinate columns in a known CRS or
#' an sf points data frame
#' 
#' @param xcoord The x coordinate column if using a raw data
#' frame
#'  
#' @param ycoord The y coordinate column if using a raw data 
#' frame
#' 
#' @param crdsys The epsg code if using a raw data frame
#' 
#' @return A new sf data frame with a populated 'COMID' column
#' @export
#'
#' @examples
#' comids <- sc_get_comid(df=df, xcoord='lon_dd', 
#' ycoord='lat_dd', crdsys=4269)
#' 
#' comids <- sc_get_comid(df=df)
#' 

sc_get_comid <- function(df = NULL, xcoord = NULL, 
                         ycoord=NULL, crdsys=NULL) {
  if (!'sf' %in% class(df) & ((is.null(xcoord)) | 
                                    (is.null(ycoord)) |
                                     (is.null(crdsys)))) {
    "\nMake sure you supply parameters for xcoord, ycoord, and a crs as an epsg code."
  } else {
    df <- df %>%
      sf::st_as_sf(coords = c(xcoord, ycoord), crs = crdsys, remove = FALSE)
  }
  
  run_for <- 1:nrow(df)
  output <- do.call(rbind, lapply(1:nrow(df), function(i){
    comid <- nhdplusTools::discover_nhdplus_id(df[i,c('geometry')])
    if (length(comid)==0L) comid <- NA else comid <- comid
    return(comid)
  }))
  output <- as.data.frame(output)
  names(output)[1] <- 'COMID'
  if (any(is.na(output$COMID))){
    missing <- which(is.na(output$COMID))
    message(cat('The following rows in the input file came back with no corresponding \nCOMIDS, likely because the sites were outside of the \nNHDPlus COMID features: ',as.character(missing)))
  }
  comids <- cbind(df, output)
  return(comids)
}
