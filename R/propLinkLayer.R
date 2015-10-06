#' @name propLinkLayer
#' @title Proportional Links Layer
#' @description Plot a layer of proportional links. Links widths are directly proportional to values of a variable.
#' @param spdf a SpatialLinesDataFrame; a link layer.
#' @param df a data frame with identifiers and a variable.
#' @param spdfid unique identifier in spdf (spdfids, spdfide, dfids and dfide are not used).
#' @param spdfids identifier of starting points in spdf (spdfid and dfid are not used).
#' @param spdfide identifier of ending points in spdf (spdfid and dfid are not used).
#' @param dfid unique identifier in df (spdfids, spdfide, dfids and dfide are not used).
#' @param dfids identifier of starting points in df (spdfid and dfid are not used).
#' @param dfide identifier of ending points in df (spdfid and dfid are not used).
#' @param var name of the variable used to plot the links widths.
#' @param maxlwd maximum size of the links.
#' @param col color of the links.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values 
#' displayed in the legend.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @note Unlike most of cartography functions, identifiers fields are mandatory.
#' @import sp
#' @seealso \link{gradLinkLayer}, \link{getLinkLayer}, \link{legendPropLines}
#' @examples
#' data("nuts2006")
#' # Create a link layer of the twin cities agreements
#' twincities.spdf <- getLinkLayer(spdf = nuts2.spdf, df = twincities[,1:2])
#' # Plot the links - Twin cities agreements between regions 
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' propLinkLayer(spdf = twincities.spdf, df = twincities[twincities$fij>=5,],maxlwd = 10,
#'               spdfids = "i", spdfide = "j",
#'               dfids = "i", dfide = "j",legend.pos = "topright",
#'               var = "fij", 
#'               col = "#92000090", add = TRUE)
#' @export
propLinkLayer <- function(spdf, df, spdfid = NULL, spdfids, spdfide, 
                          dfid = NULL, dfids, dfide,
                          var, maxlwd = 40, 
                          col, 
                          legend.pos = "bottomleft",  
                          legend.title.txt = var, 
                          legend.title.cex = 0.8, 
                          legend.values.cex = 0.6, 
                          legend.values.rnd = 0,
                          legend.frame = FALSE, 
                          add = TRUE){
  # joint
  if (is.null(spdfid)){
    spdf@data <- data.frame(df[match(x = paste(spdf@data[,spdfids],
                                               spdf@data[,spdfide]), 
                                     table = paste(df[,dfids], 
                                                   df[,dfide])),]) 
  } else {
    spdf@data <- data.frame(df[match(x = spdf@data[,spdfid], 
                                     table = df[,dfid]),]) 
  }
  spdf <- spdf[!is.na(spdf@data[,var]),]
  maxval <- max(spdf@data[,var])
  spdf@data$lwd <- spdf@data[,var] * maxlwd / maxval
  plot(spdf, lwd = spdf@data$lwd, col = col, add = add)
  if(legend.pos !="n"){
    legendPropLines(pos = legend.pos, title.txt = legend.title.txt, 
                    title.cex = legend.title.cex,
                    values.cex = legend.values.cex, var = spdf@data[,var], 
                    lwd = spdf@data$lwd, col = col, frame = legend.frame, 
                    values.rnd = legend.values.rnd)
  }
}
