#' @title Dot Density layer
#' @name dotDensityLayer
#' @description Plot a dot density layer.
#' @param spdf SpatialPointsDataFrame or SpatialPolygonsDataFrame; if spdf 
#' is a SpatialPolygonsDataFrame symbols are plotted on centroids.
#' @param df data frame; df contains the values to plot.
#' @param spdfid id field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid id field in df, default to the first column of df. (optional)
#' @param var name of the numeric field in df to plot.
#' @param n one dot on the map represents n (in var units).
#' @param col color of the points.
#' @param iter number of iteration to try to locate sample points (see Details).
#' @param pch symbol to use: \link{points}.
#' @param cex size of the symbols
#' @param type points allocation method: "random" or "regular" (see Details).
#' @param legend.txt text in the legend.
#' @param legend.pos 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.cex size of the legend text.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.col color of the text in the legend.
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details The iter parameter is defined within the \link{spsample} function. If an error occurred, increase this value.\cr
#' The type parameters is defined within the \link{spsample} function.
#' @export
#' @import sp
#' @seealso \link{propSymbolsLayer}
#' @examples
#' data("nuts2006")
#' # Example 1
#' plot(nuts0.spdf)
#' dotDensityLayer(spdf = nuts0.spdf, df=nuts0.df,var="pop2008")
#' 
#' 
#' # Example 2
#' layoutLayer(title = "Population in Europe, 2008", 
#'             sources = "Eurostat, 2008", 
#'             scale = NULL, 
#'             frame = TRUE,
#'             col = "black", 
#'             coltitle = "white",
#'             bg = "#E6E6E6", 
#'             extent = nuts0.spdf)
#' plot(nuts1.spdf, col = "#B8704D50",border = "white", add=TRUE)
#' dotDensityLayer(spdf = nuts1.spdf, df=nuts1.df,var="pop2008",
#'                 type = "regular", pch=20, col = "brown",
#'                 n = 100000)
dotDensityLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var,
                            n = NULL, # Un point vaut nb units
                            iter = 5,
                            pch = 1,
                            cex = .15,
                            type = "random",
                            col = "black",
                            legend.pos = "topright",
                            legend.txt = NULL,
                            legend.cex = 0.6,
                            legend.col = "black",
                            legend.frame = TRUE,
                            add = TRUE){
  
  
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  spdf@data <- data.frame(spdf@data, var = df[match(spdf@data[,spdfid], 
                                                    df[,dfid]),var])
  spdf@data[is.na(spdf@data$var),"var"] <- 0
  if (is.null(n)){n <- round(sum(spdf@data$var)/5000,-3)}
  spdf@data$ndots <- as.integer(spdf@data$var/n)
  spdf <- spdf[spdf@data$ndots>0,]
  
  if (add==FALSE){
    sp::plot(spdf, col = NA, border = NA)
  }
  
  
  for (i in 1:nrow(spdf)){
    #points(spsample(spdf[i,], n = spdf@data[i,"ndots"], type = dots.type), pch = dots.pch, cex= dots.cex)
    plot(spsample(spdf[i,], n = spdf@data[i,"ndots"], type = type, 
                  iter = iter), pch = pch, cex= cex , col=col, 
         add=TRUE)
  }
  
  if(legend.pos !="n"){
    
    if (is.null(legend.txt)){legend.txt <- paste("1 dot represents ",
                                                 format(n, scientific = FALSE),
                                                 " [in ",var," units]",sep="")}
    if (legend.frame==TRUE){
      fill <-  "white"
      border <- "black"
    } else {
      fill <- NA
      border <- NA
    }
    
    
    legend(legend = legend.txt,cex = legend.cex, text.col = legend.col, 
           pch=pch ,pt.cex=cex, x=legend.pos, box.col = border,
           bg = fill)
    
  }
}
