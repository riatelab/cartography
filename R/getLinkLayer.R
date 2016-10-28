#' @name getLinkLayer
#' @title Create a SpatialLinesDataFrame from a Data Frame of Links.
#' @description Create a SpatialLinesDataFrame from a data frame of links.
#' @param spdf a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; layer used to 
#' get starting points of links. If spdf2 is NULL, spdf is also used to get ending points.  
#' If spdf is a SpatialPolygonsDataFrame, links start (or end) at centroids.
#' @param spdf2 a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; layer used to 
#' get ending points of links. 
#' If spdf2 is a SpatialPolygonsDataFrame, links start (or end) at centroids. (optional)
#' @param df a data frame that contains identifiers of starting and ending points.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param spdf2id identifier field in spdf2, default to the first column 
#' of the spdf2 data frame. (optional)
#' @param dfids identifier field of starting points of links in df, default to the first column 
#' of df. (optional)
#' @param dfide identifier field of ending points of links in df, default to the second column 
#' of df. (optional)
#' @return A SpatialLinesDataFrame is returned, its data frame contains two fields (dfids and dfide).
#' @import sp
#' @examples 
#' data("nuts2006")
#' # Create a link layer
#' head(twincities.df)
#' # Select links from Ireland (IE)
#' twincitiesIE <- twincities.df[substr(twincities.df$i,1,2)=="IE", ]
#' twincities.spdf <- getLinkLayer(spdf = nuts2.spdf, df = twincitiesIE[,1:2])
#' # Plot the links
#' plot(nuts2.spdf, col = "#6C6870")
#' plot(twincities.spdf, col = "#F78194", add = TRUE)
#' @seealso \link{gradLinkLayer}, \link{propLinkLayer}
#' @export
getLinkLayer <- function(spdf, spdf2 = NULL, df, 
                          spdfid = NULL, spdf2id = NULL, 
                          dfids = NULL, dfide = NULL){
  
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfids)){dfids <- names(df)[1]}
  if (is.null(dfide)){dfide <- names(df)[2]}
  
  origin <- data.frame (sp::coordinates(spdf), id = spdf@data[,spdfid])
  names(origin) <- c("xOri", "yOri", "idOri")
  
  if (!is.null(spdf2)){
    if (is.null(spdf2id)){spdf2id <- names(spdf2@data)[1]}
    destination <- data.frame (sp::coordinates(spdf2), id = spdf2@data[,spdf2id])
  }else{
    destination <- data.frame (sp::coordinates(spdf), id = spdf@data[,spdfid])
  }
  names(destination) <- c("xDes", "yDes", "idDes")
  
  df <- df[,c(dfids, dfide)]
  link <- merge(df, origin, by.x = dfids, by.y = "idOri", all.x = TRUE)
  link <- merge(link, destination, by.x =  dfide, by.y = "idDes", all.x = TRUE)
  link <- link[!is.na(link$xOri) & !is.na(link$xDes), ]
  link$id <- paste(link[,dfids],link[,dfide], sep = "zorglub" )
  
  # if (requireNamespace("rgeos", quietly = TRUE)) {
    wkt <- paste("GEOMETRYCOLLECTION(",
                 paste("LINESTRING(",
                       link[,3]," ",link[,4],",",
                       link[,5]," ",link[,6], 
                       sep="", collapse = "),"),
                 "))", sep ="")
    sl <- rgeos::readWKT(wkt)
    sl@proj4string <- spdf@proj4string
  # }else{
  #   getMyLines <- function(x){
  #     myLine <- Line(matrix(ncol = 2, data = as.numeric(x[3:6]), byrow = TRUE))
  #     myLines <- Lines(list(myLine), ID = paste(x[dfids],x[dfide], sep = "zorglub" ))
  #   }
  #   myLinesList <- apply(X = link, 1, getMyLines)
  #   sl <- SpatialLines(LinesList = myLinesList, 
  #                      proj4string = (spdf@proj4string))
  #   cat("'rgeos' is not installed.\n")
  #   cat("'getLinkLayer' works much faster with 'rgeos' installed.")
  # }
  
  
  myspdf <- SpatialLinesDataFrame(sl = sl, data = link[,2:1],
                                  match.ID = FALSE)
  return(myspdf)
}
