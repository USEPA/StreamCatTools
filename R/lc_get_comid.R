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
#' (simply passed to hydrogeofetch::get_waterbodies)
#' 
#' @return A new sf data frame with a populated 'COMID' column
#'
#' @examples
#' \dontrun{
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

  output <- vapply(seq_len(nrow(dd)), function(i) {
    res <- tryCatch({
      if (is.null(buffer)) {
        hydrogeofetch::get_waterbodies(dd[i, ])
      } else {
        hydrogeofetch::get_waterbodies(dd[i, ], buffer = buffer)
      }
    }, error = function(e) NULL)

    if (is.null(res) || !inherits(res, "sf") || nrow(res) == 0L) {
      return(NA_character_)
    }

    comids <- tryCatch({
      unique(as.character(dplyr::pull(res, comid)))
    }, error = function(e) character(0))

    if (length(comids) == 0L || all(is.na(comids))) {
      return(NA_character_)
    }

    paste(comids, collapse = ",")
  }, character(1))

  output_df <- data.frame(COMID = output, stringsAsFactors = FALSE)

  if (any(is.na(output_df$COMID))) {
    missing <- which(is.na(output_df$COMID))
    message(paste0('Row number ', paste(as.character(missing), collapse = ", "), ' came back with no corresponding COMIDS because the site(s) were outside the boundary of any NHDPlus Waterbody features. Any NA values in this list of COMIDs will be dropped by default in lc_get_data()'))
  }

  comids <- paste(output_df$COMID, collapse = ',')
  return(comids)
}
