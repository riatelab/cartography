#' @title labelLayer function.
#' @description Put labels
#' @name labelLayer
#' @param spdf Spatial*DataFrame.
#' @param df data.frame; \code{df} contains the labels field to plot.
#' @param spdfid character; id field in \code{spdf}, default to the first column 
#' of the \code{spdf} data.frame. (optional)
#' @param dfid character; id field in \code{df}, default to the first column 
#' of \code{df}. (optional)
#' @param txt character; labels field in \code{df}.
#' @param col character; labels color.
#' @param cex numeric; labels cex.
#' @param ... further \link{text} arguments, such as \code{pos} or \code{adj}.
#' @export
#' @import sp
#' @examples
#' data("nuts2006")
#' 
#' # Display countries boundaries
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' 
#' # Layout plot
#' layoutLayer(title = "Most Populated Countries of Europe", 
#'             sources = "The 10 most populated countries of Europe", 
#'             author = "Total population 2008, in millions of inhabitants.",
#'             scale = NULL, 
#'             frame = TRUE,
#'             col = "black", 
#'             coltitle = "white",
#'             south = TRUE)
#' 
#' # Selection of the 10 most populated countries of Europe
#' dflab <- nuts0.df[order(nuts0.df$pop2008, decreasing = TRUE),][1:10,]
#' 
#' # Label creation 
#' dflab$lab <- paste(dflab$id, "\n", round(dflab$pop2008/1000000,0), "M", sep ="")
#' 
#' # Label plot of the 10 most populated countries
#' labelLayer(spdf = nuts0.spdf, df = dflab, txt = "lab", 
#'            col = "blue", cex = 0.7, font = 2)
labelLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, txt, col = "black",
                       cex = 0.7, ...){
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  if (class(spdf) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")){
    dots <- data.frame(id = spdf@data[, spdfid],coordinates(spdf))
    colnames(dots) <- c(spdfid,"x","y")
    dots <- data.frame(dots[,c("x", "y")], 
                       txt = df[match(dots[ ,spdfid], df[ , dfid]), txt])
    text(dots$x, dots$y, labels = dots[,"txt"], cex=cex, col=col, ...)
  }
}
