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
#' @param dd Name of data frame object.  Can be a simple
#' data frame with coordinate columns in a known CRS or
#' an sf points data frame
#' 
#' @param xcoord The x coordinate column if using a raw data
#' frame
#'  
#' @param ycoord The y coordinate column if using a raw data 
#' frame
#' 
#' @param crsys The epsg code if using a raw data frame
#' 
#' @return A new sf data frame with a populated 'COMID' column
#'
#' @examples
#' \dontrun{
#' 
#' dd <- data.frame(x = c(-122.649,-100.348,-75.186,-106.675),
#' y = c(45.085, 35.405,42.403,38.721))
#' 
#' comids <- sc_get_comid(dd, xcoord='x',
#'                        ycoord='y', crsys=4269)
#' 
#' dd <- sf::st_point_on_surface(sf::read_sf(system.file("shape/nc.shp", package="sf")))
#' 
#' comids <- sc_get_comid(dd)
#'                                   
#' comids <- sc_get_comid(dd, xcoord='x', 
#' ycoord='y', crsys=4269)
#' 
#' dd <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
#' comids <- sc_get_comid(dd)
#' 
#'  }
#' @export

sc_get_comid <- function(dd = NULL, xcoord = NULL, 
                         ycoord=NULL, crsys=NULL) {
  if (!'sf' %in% class(dd) & ((is.null(xcoord)) | 
                                    (is.null(ycoord)) |
                                     (is.null(crsys)))) {
    "\nMake sure you supply parameters for xcoord, ycoord, and a crs as an epsg code."
  } else {
    dd <- sf::st_as_sf(dd, coords = c(xcoord, ycoord), crs = crsys, remove = FALSE)
  }
  geom_col <- attr(dd, "sf_column")
  run_for <- 1:nrow(dd)
  output <- do.call(rbind, lapply(1:nrow(dd), function(i){
    comid <- nhdplusTools::discover_nhdplus_id(dd[i,c(geom_col)])
    if (length(comid)==0L) comid <- NA else comid <- comid
    return(comid)
  }))
  output <- as.data.frame(output)
  names(output)[1] <- 'COMID'
  if (any(is.na(output$COMID))){
    missing <- which(is.na(output$COMID))
    message(paste0('Row number ', as.character(missing), ' came back with no corresponding COMIDS because the site(s) were outside the boundary of any NHDPlus Waterbody features. Any NA values in this list of COMIDs will be dropped by default in sc_get_data()'))
  }
  comids <- paste(output$COMID, collapse=',')
  return(comids)
}
