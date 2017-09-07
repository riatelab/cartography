#' @title Label Layer
#' @description Put labels on a map.
#' @name labelLayer
#' @param x an sf object, a simple feature collection. 
#' @param spdf  a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame texts are plotted on centroids.
#' @param df a data frame that contains the labels to plot. If df is missing 
#' spdf@data is used instead. 
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param txt labels field in df.
#' @param col labels color.
#' @param cex labels cex.
#' @param ... further \link{text} arguments, such as pos or adj.
#' @export
#' @seealso \link{layoutLayer}
#' @import sp
#' @examples
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' plot(st_geometry(mtq), col = "olivedrab1", border = "olivedrab4", 
#'      bg = "lightsteelblue3")
#' labelLayer(x = mtq[mtq$STATUT != "Commune simple",], txt = "LIBGEO")
#' 
#' 
#' data("nuts2006")
#' #' # Layout plot
#' layoutLayer(  title = "Most Populated Countries of Europe", 
#'               author = "", sources = "",
#'               scale = NULL,col = NA, coltitle = "black",
#'               frame = FALSE, bg = "#A6CAE0",
#'               south = TRUE, extent = nuts0.spdf)
#' 
#' plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
#' plot(nuts0.spdf, col = "#D1914D",border = "white", lwd=1, add=TRUE)
#' 
#' # Selection of the 10 most populated countries of Europe
#' dflab <- nuts0.df[order(nuts0.df$pop2008, decreasing = TRUE),][1:10,]
#' 
#' # Label creation 
#' dflab$lab <- paste(dflab$id, "\n", round(dflab$pop2008/1000000,0), "M", sep ="")
#' 
#' # Label plot of the 10 most populated countries
#' labelLayer(spdf = nuts0.spdf, df = dflab, txt = "lab", 
#'            col = "#690409", cex = 0.9, font = 2)
#' text(x = 5477360, y = 4177311, labels = "The 10 most populated countries of Europe
#' Total population 2008, in millions of inhabitants.",
#'      cex = 0.7, adj = 0)
labelLayer <- function(x, spdf, df, spdfid = NULL, dfid = NULL, txt, col = "black",
                       cex = 0.7, ...){
  if (missing(x)){
    x <- convertToSf(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid)
  }
  text(sf::st_coordinates(sf::st_centroid(x = x, of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))), 
       labels = x[[txt]], cex = cex, col = col, ...)
}

#' Title
#'
#' @param x t
#' @param cex t
#' @param show.lines t
#' @param ... t
#' @param spdf c
#' @param df c
#' @param spdfid c
#' @param dfid c
#' @param txt c
#' @param col c
#' @param bg d
#' @param r d
#' @param overlap df
#' @param halo fd
#'
#' @return dgjsdfj 
#' @export
#' @examples
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' plot(st_geometry(mtq), col = "olivedrab1", border = "olivedrab4", 
#'      bg = "lightsteelblue3")
#' labelLayer2(x = mtq, txt = "LIBGEO")
labelLayer2 <- function(x, spdf, df, spdfid = NULL, dfid = NULL, txt, col = "black",
                        cex = 0.7, overlap = TRUE, show.lines = TRUE, 
                        halo = FALSE, bg = "white", r = 0.1, ...){
  if (missing(x)){
    x <- convertToSf(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid)
  }
  words = x[[txt]]
  cc <- sf::st_coordinates(sf::st_centroid(x = x, of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON"))))
  
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
          points(x[i],y[i],pch=16,col="grey",cex=.5)
          nx <- xl+.5*w
          ny <- yl+.5*h
          lines(c(x[i],nx),c(y[i],ny), col="grey", lwd = 1)
        }
      }
    }
    
    cc <- matrix(data = c(lay[,1]+.5*lay[,3],  lay[,2]+.5*lay[,4]), ncol = 2, byrow = FALSE)
    
  }
  
  if (halo){
    shadowtext(x = cc[,1], y = cc[,2], labels = words,
             cex = cex, col = col, bg = bg, r = r, ...)
  }else{
    text(x = cc[,1], y = cc[,2], labels = words, cex = cex, col = col, ...)
  }

}

