#' @title Dot Density Layer
#' @name dotDensityLayer
#' @description Plot a dot density layer.
#' @param x an sf object, a simple feature collection. If x is used then spdf,
#'  df, spdfid and dfid are not. 
#' @param spdf a SpatialPolygonsDataFrame. 
#' @param df a data frame that contains the values to plot. If df is missing 
#' spdf@data is used instead. 
#' @param spdfid name of the identifier variable in spdf, default to the first 
#' column of the spdf data frame. (optional)
#' @param dfid name of the identifier variable in df, default to the first 
#' column of df. (optional)
#' @param var name of the numeric variable to plot.
#' @param n one dot on the map represents n (in var units).
#' @param col color of the points.
#' @param iter defunct.
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
#' @details 
#' The type parameters is defined within the \link{st_sample} function.
#' @export
#' @seealso \link{tc_map_dd}
#' @keywords internal
#' @examples
#' \dontrun{
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq), col = "#B8704D50")
#' dotDensityLayer(x = mtq,  var="POP", pch=20, col = "red4", n = 200)
#' layoutLayer(title = "Population Distribution in Martinique, 2015")
#' }
dotDensityLayer <- function(x, spdf, df, spdfid = NULL, dfid = NULL, var,
                            n = NULL, 
                            pch = 1,
                            cex = .15,
                            type = "random",
                            col = "black",
                            iter,
                            legend.pos = "topright",
                            legend.txt = NULL,
                            legend.cex = 0.6,
                            legend.col = "black",
                            legend.frame = TRUE,
                            add = TRUE){
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::dotDensityLayer()",
                            with = "tc_map_dd()") 
  if (!missing(iter)){
      stop("iter argument is defunct", call. = FALSE)
  }
  
  if (missing(x)){
    x <- convertToSf(spdf = spdf, df = df, spdfid = spdfid, dfid = dfid)
  }
  
  if (is.null(n)){
    n <- round(min(x[[var]], na.rm = TRUE), 0) + 1
  }

  x <- x[!is.na(x[[var]]), var]
  x$ndots <- as.integer(x[[var]]/n)
  x <- x[x$ndots > 0,]
  
  if (add == FALSE){
    ghostLayer(x)
  }

  plot(sf::st_sample(x = x, size = x[["ndots"]], type = type, exact = TRUE), 
       pch = pch, cex = cex , col = col, add = TRUE)

  if(legend.pos !="n"){
    if (is.null(legend.txt)){
      legend.txt <- paste0("1 dot represents ", format(n, scientific = FALSE),
                          " [in ",var," units]")
    }
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
