#' @title Get Lake COMIDs
#' 
#' @description 
#' Function to return NHDPlusV2 Waterbody COMIDS using either a 
#' dataframe with coordinates and a specified CRS or an 
#' sf object.  The function generates a vector of NHDPlus Waterbody COMID
#' values a user can then pass to lc_get_data function
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
#' @param buffer The amount of buffer to use to extend search for a waterbody 
#' (simply passed to nhdplusTools::get_waterbodies)
#' 
#' @return A new sf data frame with a populated 'COMID' column
#'
#' @examples
#' \donttest{
#' 
#' dd <- data.frame(x = c(-89.198,-114.125,-122.044),
#' y = c(45.502,47.877,43.730))
#' 
#' comids <- lc_get_comid(dd, xcoord='x',
#'                        ycoord='y', crsys=4269)
#' 
#' dd <- data.frame(x = c(-89.198,-114.125,-122.044),
#' y = c(45.502,47.877,43.730)) |> 
#'    sf::st_as_sf(coords = c('x', 'y'), crs = 4326)
#'    
#' comids <- lc_get_comid(dd)
#'  }
#' @export

lc_get_comid <- function(dd = NULL, xcoord = NULL, 
                         ycoord=NULL, crsys=NULL, buffer=NULL) {
  if (!'sf' %in% class(dd) & ((is.null(xcoord)) | 
                                    (is.null(ycoord)) |
                                     (is.null(crsys)))) {
    "\nMake sure you supply parameters for xcoord, ycoord, and a crs as an epsg code."
  } else {
    dd <- sf::st_as_sf(dd, coords = c(xcoord, ycoord), crs = crsys, remove = FALSE)
  }
  
  
  output <- do.call(rbind, lapply(1:nrow(dd), function(i){
    if (is.null(buffer)){
      wb <- nhdplusTools::get_waterbodies(dd[i,])
    } else {
      wb <- nhdplusTools::get_waterbodies(dd[i,], buffer=buffer)
    }
    if (!is.null(wb)){
      comid <- wb |>
        dplyr::pull(comid)
    if (length(comid)==0L) comid <- NA else comid <- comid
      return(comid)
    } 
  }))
  output <- as.data.frame(output)
  names(output)[1] <- 'COMID'
  if (any(is.na(output$COMID))){
    missing <- which(is.na(output$COMID))
    message(cat('The following row(s) in the input file came back with no corresponding COMIDS: ',as.character(missing),'\n 
    because the site(s) were outside the boundary of any NHDPlus Waterbody features. Any NA values in\n 
    this list of COMIDs will be dropped by default in lc_get_data()'))
  }
  comids <- paste(output$COMID, collapse=',')
  return(comids)
}
