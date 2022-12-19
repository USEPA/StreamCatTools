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
#' \donttest{
#' set.seed(1234)
#' pt <- 10000
#' dd <- data.frame(x = runif(pt, 0, 100),
#'                  y = runif(pt, 0,50),
#'                  f1 = rnorm(pt))
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
  
  run_for <- 1:nrow(dd)
  output <- do.call(rbind, lapply(1:nrow(dd), function(i){
    comid <- nhdplusTools::discover_nhdplus_id(dd[i,c('geometry')])
    if (length(comid)==0L) comid <- NA else comid <- comid
    return(comid)
  }))
  output <- as.data.frame(output)
  names(output)[1] <- 'COMID'
  if (any(is.na(output$COMID))){
    missing <- which(is.na(output$COMID))
    message(cat('The following rows in the input file came back with no corresponding \nCOMIDS, likely because the sites were outside of the \nNHDPlus COMID features: ',as.character(missing)))
  }
  comids <- paste(output$COMID, collapse=',')
  return(comids)
}
