#' @title staticLayer
#' @description ddd
#' @name staticLayer
#' @details Just a wrapper for sp::plot()
#' @param spdf Spatial*DataFrame
#' @param col Filling color
#' @param border Border color
#' @param lwd Border thickness
#' @param add Whether to add the layer to an existing map (TRUE) or not (FALSE)
#' @param pch pch
#' @param cex cex
#' @export
#' @import sp
#' @examples
#' data("TNdeleg")
staticLayer <- function(spdf, col = "#EFDEC1", border = "black", lwd = 1,
                        cex = 1, pch=19, add = FALSE){

  if (class(spdf)=="SpatialPolygonsDataFrame")
  {plot(spdf, border = border, col = col, lwd = lwd, add = add)}

  if (class(spdf)=="SpatialLinesDataFrame")
  {plot(spdf, col = col, lwd = lwd, add = add)}

  if (class(spdf)=="SpatialPointsDataFrame")
  {plot(spdf, col = col, cex = cex, pch = pch, add = add)}
}



#' @title labelLayer function.
#' @description Put labels
#' @name labelLayer
#' @param spdf Spatial*DataFrame
#' @param df DataFrame with Ids and Labels
#' @param spdfid Ids of the obj Spatial*DataFrame
#' @param dfid Ids of the DataFrame
#' @param txt Labels field in data
#' @param col Labels color
#' @param cex Labels size
#' @export
#' @import sp
#' @examples
#' data("TNdeleg")
labelLayer <- function(spdf, df, spdfid = NA, dfid = NA, txt, col = "black",
                       cex = 0.7){
  if (is.na(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.na(dfid)){dfid <- names(df)[1]}
  if (class(spdf) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")){
    dots <- cbind(spdf@data[, spdfid],as.data.frame(coordinates(spdf)))
    colnames(dots) <- c(spdfid,"x","y")
    dots <- data.frame(dots, df[match(dots[ ,spdfid], df[ , dfid]), ])
    text(dots$x, dots$y, labels = dots[,txt], cex=cex, col=col)
  }
}





