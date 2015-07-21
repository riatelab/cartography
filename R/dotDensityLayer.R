#' @title Dot density layer
#' @name dotDensityLayer
#' @description Plot a dot density layer.
#' @param spdf SpatialPointsDataFrame or SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df data.frame; df contains the values to plot.
#' @param spdfid character; id field in spdf, default to the first column 
#' of the spdf data.frame. (optional)
#' @param dfid character; id field in df, default to the first column 
#' of df. (optional)
#' @param var character; name of the numeric field in df to plot.
#' @param n numeric; one dot on the map represents n [in var units]
#' @param col character; color of the symbols
#' @param iter numeric;  number of times to try to place sample points (see Details)
#' @param dots.pch numeric; Symbol to use:  \link{points}.
#' @param dots.cex numeric; size of the symboles
#' @param dots.type character; Points allocation method: "random", "regular", "stratified", "nonaligned", "hexagonal", "clustered", "Fibonacci". (see Details)
#' @param legend.txt character; text in the legend.
#' @param legend.pos 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.cex numeric; size of the legend textes.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.col character; color of the text within the legend textbox.
#' @param add boolean; whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details The iter parameter is defined within the \link{spsample} function. If an error occurred, increase this value.
#' The dots.type parameters is defined within the \link{spsample} function.
#' @export
#' @import sp
#' @examples
#' data("nuts2006")
#' layoutLayer(title = "Population in Europe, 2008", 
#'             sources = "Eurostat, 2008", 
#'             scale = NULL, 
#'             frame = TRUE,
#'             col = "black", 
#'             coltitle = "white",
#'             bg = "#E6E6E6", 
#'             extent = nuts0.spdf)
#' plot(nuts0.spdf, col = "#B8704D",border = "white", lwd=1.3, add=TRUE)
#' dotDensityLayer(spdf = nuts3.spdf, df=nuts3.df,var="pop2008", add=TRUE)
dotDensityLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var,
                      n = NULL, # Un point vaut nb units
                      iter = 5,
                      dots.pch = 1,
                      dots.cex = .15,
                      dots.type = "random",
                      col = "black",
                      legend.txt = NULL,
                      legend.pos = "topright",
                      legend.cex = 0.6,
                      legend.col = "black",
                      legend.frame = TRUE,
                      add = FALSE){

  
if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
if (is.null(dfid)){dfid <- names(df)[1]}

spdf@data <- data.frame(spdf@data, var = df[match(spdf@data[,spdfid], df[,dfid]),var])
spdf@data[is.na(spdf@data$var),"var"] <- 0
if (is.null(n)){n <- round(sum(spdf@data$var)/5000,-3)}
spdf@data$ndots <- as.integer(spdf@data$var/n)
spdf <- spdf[spdf@data$ndots>0,]

if (add==FALSE){
  sp::plot(spdf, col = NA, border = NA)
}


for (i in 1:nrow(spdf)){
#points(spsample(spdf[i,], n = spdf@data[i,"ndots"], type = dots.type), pch = dots.pch, cex= dots.cex)
plot(spsample(spdf[i,], n = spdf@data[i,"ndots"], type = dots.type, iter = iter), pch = dots.pch, cex= dots.cex , col=col, add=TRUE)
}

if(legend.pos !="n"){

  
  if (is.null(legend.txt)){legend.txt <- paste("1 dot represents ",format(n, scientific = FALSE)," [in ",var," units]",sep="")}
  if (legend.frame==TRUE){fill = "white" 
                          border="black"} else {fill = NA
                                                border=NA}
     
  
legend(legend = legend.txt,cex = legend.cex, text.col = legend.col, pch=dots.pch ,pt.cex=dots.cex, x=legend.pos, box.col = border,bg = fill)

}


}


