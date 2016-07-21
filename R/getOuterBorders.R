#' @title Extract SpatialPolygonsDataFrame Outer Borders
#' @description Extract outer borders between SpatialPolygonsDataFrame units. 
#' Outer borders are non-contiguous SpatialPolygonsDataFrame borders (e.g. 
#' maritim borders).
#' @name getOuterBorders
#' @param spdf a SpatialPolygonsDataFrame. 
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param res resolution of the grid used to compute borders (in spdf units). 
#' A high resolution will give more detailed borders. (optional)
#' @param width maximum distance between used to compute borders (in spdf units). 
#' A higher width will build borders between units that are farther apart. (optional)
#' @return A SpatialLinesDataFrame of borders is returned. This object has three 
#' id fields: id, id1 and id2.
#' id1 and id2 are ids of units that neighbour a border; id is the concatenation 
#' of id1 and id2 (with "_" as separator).
#' @note getBorders and getOuterBorders can be combined with rbind. 
#' @examples
#' \dontrun{
#' data(nuts2006)
#' # Get units borders
#' nuts0.outer <- getOuterBorders(nuts0.spdf)
#' # Plot Countries
#' plot(nuts0.spdf, border = NA, col = "grey60")
#' # Plot borders
#' plot(nuts0.outer, col = sample(x = rainbow(nrow(nuts0.outer))), 
#'      lwd = 3, add = TRUE)
#' }
#' @seealso \link{discLayer}, \link{getBorders} 
#' @export
getOuterBorders <- function(spdf, 
                            spdfid = NULL, 
                            res = NULL, 
                            width = NULL){
  
  if (is.null(spdfid)) {
    spdfid <- names(spdf@data)[1]
  }
  if(!is.numeric(spdf[,spdfid])){
    spdf$idxd <- 1:nrow(spdf)
  }else{
    spdf$idxd <- spdf@data[, spdfid]
  }
  
  
  boundingBox <- sp::bbox(spdf)
  w <- (boundingBox[1,2] - boundingBox[1,1]) 
  h <- (boundingBox[2,2] - boundingBox[2,1])
  
  
  if(is.null(res)){
    res <- round(max(c(w, h))/150,0)
  }

  if(is.null(width)){
    width <- round(max(c(w, h))/20,0)
  }
  
  
  # Create raster of spdf
  ex <- raster::extent(spdf)
  ex[1] <- ex[1] - width
  ex[2] <- ex[2] + width
  ex[3] <- ex[3] - width
  ex[4] <- ex[4] + width
  
  r <- raster::raster(ex, resolution = res)
  
  r <- raster::rasterize( x= spdf,  y=r, field = 'idxd')
  
  dist <- raster::distance(r)  
  dist[dist > width] <- NA
  # you can also set a maximum distance: dist[dist > maxdist] <- NA
  direct <- raster::direction(r, from=FALSE)
  
  # NA raster
  rna <- is.na(r) # returns NA raster
  
  # store coordinates in new raster: http://stackoverflow.com/a/35592230/3752258 
  na.x <- raster::init(rna, 'x')
  na.y <- raster::init(rna, 'y')
  
  
  # calculate coordinates of the nearest Non-NA pixel
  # assume that we have a orthogonal, projected CRS, so we can use (Pythagorean) calculations
  co.x <- na.x + dist * sin(direct)
  co.y <- na.y + dist * cos(direct)
  
  # matrix with point coordinates of nearest non-NA pixel
  co <- cbind(co.x[], co.y[]) 
  
  
  # extract values of nearest non-NA cell with coordinates co
  NAVals <- raster::extract(r, co, method='simple') 
  r.NAVals <- rna # initiate new raster
  r.NAVals[] <- NAVals # store values in raster
  
  pB <- raster::rasterToPolygons(r.NAVals, dissolve = T)
  pB@data$id <- spdf@data[pB@data$layer, spdfid]
  pBBorder <- getBorders(pB, spdfid = "id" )
  
  pBBorderS <- rgeos::gSimplify(spgeom = pBBorder, tol = res)
  
  result <- sp::SpatialLinesDataFrame(sl = pBBorderS, data = pBBorder@data)
  
  row.names(result) <- paste0(row.names(result), "_o")

  return(result)                           
}                            


