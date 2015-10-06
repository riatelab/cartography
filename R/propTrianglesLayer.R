#' @title Double Proportional Triangle Layer
#' @description Plot a double proportional triangles layer.
#' @name propTrianglesLayer
#' @param spdf a SpatialPointsDataFrame or a SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df a data frame that contains the values to plot.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var1 name of the first numeric field in df to plot, positive values 
#' only (top triangle).
#' @param var2 name of the second  numeric field in df to plot, positive values 
#' only (bottom triangle).
#' @param col1 color of top triangles.
#' @param col2 color of bottom triangles.
#' @param k share of the map occupied by the biggest symbol.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.var1.txt label of the top variable.
#' @param legend.var2.txt label of the bottom variable.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values 
#' displayed in the legend.
#' @param legend.style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @export
#' @import sp
#' @seealso \link{legendPropTriangles}
#' @examples
#' data("nuts2006")
#' # Example 1
#' plot(nuts0.spdf)
#' # There is no data for deaths in Turkey
#' propTrianglesLayer(spdf = nuts0.spdf, df = nuts0.df, 
#'                    var1 = "birth_2008",
#'                    var2 = "death_2008")
#' 
#' # Example 2
#' layoutLayer(title = "Births and Deaths in Europe, 2008",
#'             sources = "",author = "",
#'             scale = NULL,
#'             frame = FALSE,
#'             col = "black",
#'             coltitle = "white",
#'             extent = nuts0.spdf)
#' plot(countries.spdf,col="#E0E0E0",border="white",lwd=1, add=TRUE)
#' plot(nuts0.spdf,col="#E5CFC1",border="white",lwd=2,add=TRUE)
#' # There is no data for deaths in Turkey
#' propTrianglesLayer(spdf = nuts0.spdf, df = nuts0.df, 
#'                    var1 = "birth_2008", legend.style = "e",
#'                    var2 = "death_2008", legend.frame = TRUE,
#'                    col1="#FF9100",col2="#45C945",k = 0.1, add=TRUE)
#' @export
propTrianglesLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, 
                               var1, col1 = "#E84923",
                               var2, col2 = "#7DC437", 
                               k = 0.02, 
                               legend.pos = "topright", 
                               legend.title.txt = paste(var1,var2,sep=" / "),
                               legend.title.cex = 0.8, 
                               legend.var1.txt = var1, 
                               legend.var2.txt = var2, 
                               legend.values.cex = 0.6,
                               legend.values.rnd = 0, 
                               legend.style = "c", 
                               legend.frame = FALSE,
                               add = TRUE)
{
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  dots <- cbind(spdf@data[, spdfid], as.data.frame(coordinates(spdf)))
  colnames(dots) <- c(spdfid,"x","y")
  dots <- data.frame(dots, df[match(dots[,spdfid], df[,dfid]),])
  dots <- dots[,c(spdfid,"x","y", var1, var2)]
  
  # Ajustage de la atille des cercles
  x1 <- sp::bbox(spdf)[1]
  y1 <- sp::bbox(spdf)[2]
  x2 <- sp::bbox(spdf)[3]
  y2 <- sp::bbox(spdf)[4]
  if (sum(df[,var1], na.rm = TRUE)>=sum(df[,var2], na.rm = TRUE)){
    var <- var1
  } else {
    var <- var2
  }
  
  sfdc <- (x2-x1)*(y2-y1)
  # sc <- sum(abs(dots[,var]),na.rm = TRUE)
  sc <- max(abs(dots[,var]),na.rm = TRUE)
  if(add==FALSE){plot(spdf)}
  
  # TRIANGLE TOP
  dots$size1 <-  sqrt(dots[,var1]*k* sfdc / sc /2)
  dots$y1<-dots$y+dots$size1/2
  dots <- dots[order(dots[,var1],decreasing=TRUE),]
  dots$xx1a<-dots$x-dots$size1/2
  dots$xx1b<-dots$x
  dots$xx1c<-dots$x+dots$size1/2
  dots$yy1a<-dots$y
  dots$yy1b<-dots$y+dots$size1/2
  dots$yy1c<-dots$y
  for (i in 1:length(dots$x)){polygon(c(dots$xx1a[i],dots$xx1b[i],dots$xx1c[i]), 
                                      c(dots$yy1a[i],dots$yy1b[i],dots$yy1c[i]), 
                                      col = col1, border = "#DDDDDD")}
  
  # TRIANGLE BOTTOM
  dots$size2 <-  sqrt(dots[,var2]*k* sfdc / sc /2)
  dots$y1<-dots$y+dots$size2/2
  dots <- dots[order(dots[,var2],decreasing=TRUE),]
  dots$xx1a<-dots$x-dots$size2/2
  dots$xx1b<-dots$x
  dots$xx1c<-dots$x+dots$size2/2
  dots$yy1a<-dots$y
  dots$yy1b<-dots$y-dots$size2/2
  dots$yy1c<-dots$y
  for (i in 1:length(dots$x)){polygon(c(dots$xx1a[i],dots$xx1b[i],dots$xx1c[i]), 
                                      c(dots$yy1a[i],dots$yy1b[i],dots$yy1c[i]), 
                                      col = col2, border = "#DDDDDD")}
  
  
  if(legend.pos!="n"){
    legendPropTriangles(pos = legend.pos, title.txt = legend.title.txt, 
                        var.txt = legend.var1.txt, 
                        var2.txt = legend.var2.txt, 
                        title.cex = legend.title.cex, 
                        values.cex = legend.values.cex,
                        var = dots[,var1],
                        var2 = dots[,var2],
                        r = dots$size1,
                        r2 = dots$size2,
                        col = col1,
                        col2 = col2,
                        frame = legend.frame,
                        values.rnd =  legend.values.rnd,
                        style = legend.style)
  }
  
}


