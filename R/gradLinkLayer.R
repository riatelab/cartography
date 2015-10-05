#' @name gradLinkLayer
#' @title Graduated Links Layer
#' @description Plot a layer of graduated links. Links are plotted according to discrete classes of widths.
#' @param spdf SpatialLinesDataFrame; a link layer.
#' @param df data frame with identifier(s) and a variable.
#' @param spdfid unique identifier in spdf (spdfids, spdfide, dfids and dfide are not used).
#' @param spdfids identifier of starting points in spdf (spdfid and dfid are not used).
#' @param spdfide identifier of ending points in spdf (spdfid and dfid are not used).
#' @param dfid unique identifier in df (spdfids, spdfide, dfids and dfide are not used).
#' @param dfids identifier of starting points in df (spdfid and dfid are not used).
#' @param dfide identifier of ending points in df (spdfid and dfid are not used).
#' @param var name of the variable used to plot the links widths.
#' @param breaks break values in sorted order to indicate the intervals for assigning the lines widths.
#' @param lwd vector of widths (classes of widths). 
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
#' @seealso \link{getLinkLayer}, \link{propLinkLayer}, \link{legendGradLines}
#' @examples
#' data("nuts2006")
#' # Create a link layer
#' twincities.spdf <- getLinkLayer(spdf = nuts2.spdf, df = twincities[,1:2])
#' # Plot the links - Twin cities agreements between regions 
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' gradLinkLayer(spdf = twincities.spdf, df = twincities,
#'               spdfids = "i", spdfide = "j",
#'               dfids = "i", dfide = "j",legend.pos = "topright",
#'               var = "fij", breaks = c(2,5,15,20,30), lwd = c(0.1,1,4,10),
#'               col = "#92000090", add = TRUE)
#' @export
gradLinkLayer <- function(spdf, df, spdfid = NULL, spdfids, spdfide, 
                          dfid = NULL, dfids, dfide,
                          var, 
                          breaks = discretization(v = df[,var],nclass = 4,
                                                  method = "quantile"), 
                          lwd = c(1,2,4,6), 
                          col = "red", 
                          legend.pos = "bottomleft",  
                          legend.title.txt = var, 
                          legend.title.cex = 0.8, 
                          legend.values.cex = 0.6, 
                          legend.values.rnd = 0,
                          legend.frame = FALSE, 
                          add = TRUE){
  # test
  if((length(breaks)-1) != length(lwd)){
    stop("length(lwd) must be equal to length(breaks) - 1",call. = FALSE)
  }
  
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
  spdf <- spdf[spdf@data[,var]>=min(breaks) & spdf@data[,var]<=max(breaks), ]
  
  # lwd
  lwdMap <- lwd[findInterval(x = spdf@data[,var], vec = breaks, all.inside = TRUE)]
  
  # map
  plot(spdf, col=col,lwd = lwdMap, add = add)
  
  # legend
  if(legend.pos !="n"){
    legendGradLines(pos = legend.pos, title.txt = legend.title.txt, 
                    title.cex = legend.title.cex ,
                    values.cex = legend.values.cex, 
                    breaks = breaks, lwd = lwd, 
                    col = col, values.rnd = legend.values.rnd,
                    frame = legend.frame)
  }
}



