



#' @name getLinkLayer
#' @title Create a SpatialLinesDataFrame from a data.frame of Links.
#' @description Create a SpatialLinesDataFrame from a data.frame of links.
#' @param spdf SpatialPointsDataFrame or SpatialPolygonsDataFrame; layer used to 
#' get starting points of links. If spdf2 is NULL, spdf is also used to get ending points.  
#' If spdf is a SpatialPolygonsDataFrame points start (or end) at centroids.
#' @param spdf2 SpatialPointsDataFrame or SpatialPolygonsDataFrame; layer used to 
#' get ending points of links. 
#' If spdf is a SpatialPolygonsDataFrame points start (or end) at centroids.
#' @param df data.frame; df contains the values to plot.
#' @param spdf2id character; id field in spdf2, default to the first column 
#' of the spdf data.frame. (optional)
#' @param dfids character; id field of starting points of links in df, default to the first column 
#' of df. (optional)
#' @param dfide character; id field of ending points of links in df, default to the second column 
#' of df. (optional)
#' @return A SpatialLinesDataFrame is returned, its data.frame contains two fields (dfid1 and dfid2).
#' @import sp
#' @examples 
#' data("nuts2006")
#' data_links <- read.csv("/mnt/data/depot/cartography/inst/extdata/data_links.csv", stringsAsFactors=FALSE)
#' x <- merge(data_links, data_links, by = "project")
#' x <- unique(x)
#' x <- x[x$nuts2.x!=x$nuts2.y,]
#' x <- unique(x[,2:3])
#' ll <- getLinkLayer(spdf = nuts2.spdf, spdf2 = nuts2.spdf, df = x)
#' plot(ll)
#' @export
getLinkLayer <- function(spdf, spdf2 = NULL, df, 
                         spdfid = NULL, spdf2id = NULL, 
                         dfids = NULL, dfide = NULL){
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfids)){dfidi <- names(df)[1]}
  if (is.null(dfide)){dfidj <- names(df)[2]}
  
  origin <- data.frame (sp::coordinates(spdf), id = spdf@data[,spdfid])
  names(origin) <- c("xOri", "yOri", "idOri")
    if (!is.null(spdf2)){
    if (is.null(spdf2id)){spdf2id <- names(spdf2@data)[1]}
    destination <- data.frame (sp::coordinates(spdf2), id = spdf2@data[,spdf2id])
  }else{
    destination <- data.frame (sp::coordinates(spdf), id = spdf@data[,spdfid])
  }
  
  names(destination) <- c("xDes", "yDes", "idDes")
  
  link <- merge(df, origin, by.x = dfidi, by.y = "idOri", all.x = TRUE)
  link <- merge(link, destination, by.x =  dfidj, by.y = "idDes", all.x = TRUE)
  link <- link[!is.na(link$xOri) & !is.na(link$xDes), ]
  
  getMyLines <- function(x){
    myLine <- Line(matrix(ncol = 2, data = as.numeric(x[3:6]), byrow = TRUE))
    myLines <- Lines(list(myLine), ID = paste(x[1],x[2], sep = "zorglub" ))
  }
  myLinesList <- apply(X = link[,],1,  getMyLines)
  mySpatialLines <- SpatialLines(LinesList = myLinesList, 
                                 proj4string = (spdf@proj4string))
  mySLDF <- SpatialLinesDataFrame(sl = mySpatialLines, data = link[,1:2], 
                                  match.ID = FALSE)
  mySLDF <- spChFIDs(obj = mySLDF, x = as.character(1:nrow(mySLDF)) )
  return(mySLDF)
}


# 
# #' @name propLinkLayer
# #' @title Proportional Links Layer
# #' @description Plot a layer of proportionnal links
# #' @param sldf SpatialLinesDataFrame; a link layer.
# #' @param df DataFrame with identifiers and a variable.
# #' @param sldfid Unique identifier in sldf (sldfids, sldfide, dfids and dfide are not used).
# #' @param sldfids Identifier of starting points in sldf (sldfid and dfid are not used).
# #' @param sldfide Identifier of ending points in sldf (sldfid and dfid are not used).
# #' @param dfid Unique identifier in df (sldfids, sldfide, dfids and dfide are not used).
# #' @param dfids Identifier of starting points in df (sldfid and dfid are not used).
# #' @param dfide Identifier of ending points in df (sldfid and dfid are not used).
# #' @param var Name of the variable used to plot the links
# #' @param maxlwd Maximum size of the links.
# #' @export
# propLinkLayer <- function(sldf, df, sldfid = NULL, sldfids, sldfide, 
#                           dfid = NULL, dfids, dfide,
#                           var, maxlwd = 40, add = T){
#   
#   data("nuts2006")
#   data_links <- read.csv("/mnt/data/depot/cartography/inst/extdata/data_links.csv", stringsAsFactors=FALSE)
#   x <- merge(data_links, data_links, by = "project")
#   x <- unique(x)
#   x <- x[x$nuts2.x!=x$nuts2.y,]
#   x$cpt <- 1
#   head(x, 50)
#   xx <- aggregate(x = data.frame(x[,"cpt"], stringsAsFactors = F), by = list(x$nuts2.x,x$nuts2.y ), FUN = sum)
#   names(xx) <- c("i", "j", "fij")
#   head(xx)
#   
#   x <- unique(x[,2:3])
#   ll <- getLinkLayer(spdf = nuts2.spdf, spdf2 = nuts2.spdf, df = x)
#   
#   
#   sldf <- ll
#   df <- xx
#   dfids <- "i"
#   dfide <- "j"
#   var <- "fij"
#   sldfids <- "nuts2.x"
#   sldfide <- "nuts2.y"  
#   
#   sldf@data <- data.frame(sldf@data, df[match(x = paste(sldf@data[,sldfids],sldf@data[,sldfide]), table = paste(df[,dfids], df[,dfide])),]) 
#   
#   
#   plot(sldf[sldf@data$fij>2,], lwd = sldf@data$fij* 2, )
#   plot(nuts2.spdf, add=T)
#   
#   if(!is.null(var2)){flux$col <- flux[,var2]}else{flux$col = "grey10"}
#   segments(x0 = flux[,"xOri"], y0 = flux[,"yOri"],
#            x1 = flux[,"xDes"], y1 = flux[,"yDes"],
#            col = flux$col,
#            lwd = flux$lwd)
# }
# 
# 
# 
# 
# 
# 
# 
