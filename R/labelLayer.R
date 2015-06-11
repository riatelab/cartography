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
