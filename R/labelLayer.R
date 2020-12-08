

#' @title Label Layer
#' @description Put labels on a map.
#' @name labelLayer
#' @param x an sf object, a simple feature collection. 
#' spdf, df, dfid and spdfid are not used. 
#' @param spdf  a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame texts are plotted on centroids.
#' @param df a data frame that contains the labels to plot. If df is missing 
#' spdf@data is used instead. 
#' @param spdfid name of the identifier variable in spdf, default to the first 
#' column of the spdf data frame. (optional)
#' @param dfid name of the identifier variable in df, default to the first 
#' column of df. (optional)
#' @param txt labels variable.
#' @param col labels color.
#' @param cex labels cex.
#' @param ... further \link{text} arguments.
#' @param bg halo color if halo is TRUE
#' @param r width of the halo
#' @param overlap if FALSE, labels are moved so they do not overlap.
#' @param halo If TRUE, then a 'halo' is printed around the text and additional 
#' arguments bg and r can be modified to set the color and width of the halo.
#' @param show.lines if TRUE, then lines are plotted between x,y and the word, 
#' for those words not covering their x,y coordinate
#' @seealso \link{tc_label}
#' @keywords internal
#' @export
#' @examples
#' library(sf)
#' opar <- par(mar = c(0,0,0,0))
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq), col = "darkseagreen3", border = "darkseagreen4", 
#'      bg = "#A6CAE0")
#' labelLayer(x = mtq, txt = "LIBGEO", col= "black", cex = 0.7, font = 4,
#'            halo = TRUE, bg = "white", r = 0.1, 
#'            overlap = FALSE, show.lines = FALSE)
#' par(opar)
labelLayer <- function(x, spdf, df, spdfid = NULL, dfid = NULL, txt,
                       col = "black",
                       cex = 0.7, overlap = TRUE, show.lines = TRUE, 
                       halo = FALSE, bg = "white", r = 0.1, ...){
  
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::labelLayer()",
                            with = "tc_label()") 
  
  if (missing(x)){
    x <- convertToSf(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid)
  }
  if (methods::is(x, 'Spatial')){
    x <- sf::st_as_sf(x)
  }
  
  words <- x[[txt]]
  cc <- sf::st_coordinates(sf::st_centroid(
    x = sf::st_geometry(x),
    of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON"))
  ))
  
  if(nrow(x) == 1){
    overlap <- TRUE
  }
  
  if (!overlap){
    x <- unlist(cc[,1])
    y <- unlist(cc[,2])
    lay <- wordlayout(x,y,words,cex)
    
    if(show.lines){
      for(i in 1:length(x)){
        xl <- lay[i,1]
        yl <- lay[i,2]
        w <- lay[i,3]
        h <- lay[i,4]
        if(x[i]<xl || x[i]>xl+w ||
           y[i]<yl || y[i]>yl+h){
          points(x[i],y[i],pch=16,col=col,cex=.5)
          nx <- xl+.5*w
          ny <- yl+.5*h
          lines(c(x[i],nx),c(y[i],ny), col=col, lwd = 1)
        }
      }
    }
    cc <- matrix(data = c(lay[,1]+.5*lay[,3],  lay[,2]+.5*lay[,4]), 
                 ncol = 2, byrow = FALSE)
  }
  if (halo){
    shadowtext(x = cc[,1], y = cc[,2], labels = words,
               cex = cex, col = col, bg = bg, r = r, ...)
  }else{
    text(x = cc[,1], y = cc[,2], labels = words, cex = cex, col = col, ...)
  }
}
