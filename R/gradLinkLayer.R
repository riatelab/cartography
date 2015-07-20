#' @name gradLinkLayer
#' @title Proportional Links Layer
#' @description Plot a layer of proportionnal links
#' @param sldf SpatialLinesDataFrame; a link layer.
#' @param df DataFrame with identifiers and a variable.
#' @param sldfid Unique identifier in sldf (sldfids, sldfide, dfids and dfide are not used).
#' @param sldfids Identifier of starting points in sldf (sldfid and dfid are not used).
#' @param sldfide Identifier of ending points in sldf (sldfid and dfid are not used).
#' @param dfid Unique identifier in df (sldfids, sldfide, dfids and dfide are not used).
#' @param dfids Identifier of starting points in df (sldfid and dfid are not used).
#' @param dfide Identifier of ending points in df (sldfid and dfid are not used).
#' @param var Name of the variable used to plot the links
#' @param breaks list des breaks
#' @param lwd list des lwd
#' @param col Color of the links.
#' @param legend.pos character; position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt character; title of the legend.
#' @param legend.title.cex numeric; size of the legend title.
#' @param legend.values.cex numeric; size of the values in the legend.
#' @param legend.values.rnd numeric; number of decimal places of the values 
#' displayed in the legend.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add boolean; whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details Unlike most of cartography functions, identifiers fields are mandatory.
#' @import sp
#' @examples
#' data("nuts2006")
#' data_links <- read.csv("/mnt/data/depot/cartography/inst/extdata/data_links.csv",
#'                        stringsAsFactors=FALSE)
#' x <- merge(data_links, data_links, by = "project")
#' x <- unique(x)
#' x <- x[x$nuts2.x!=x$nuts2.y,]
#' x$cpt <- 1
#' xx <- aggregate(x = data.frame(x[,"cpt"], stringsAsFactors = F),
#'                 by = list(x$nuts2.x,x$nuts2.y ), FUN = sum)
#' names(xx) <- c("i", "j", "fij")
#' x <- unique(x[,2:3])
#' ll <- getLinkLayer(spdf = nuts2.spdf, spdf2 = nuts2.spdf, df = x)
#' xxx <- xx
#' gradLinkLayer(sldf = ll, df = xxx,
#'               sldfids = "nuts2.x", sldfide = "nuts2.y",
#'               dfids = "i", dfide = "j",legend.pos = "n",
#'               var = "fij", breaks = c(1,2,3,6,8), lwd = c(1,3,7,100),
#'               col = "#92000050", add = FALSE)
#' @export
gradLinkLayer <- function(sldf, df, sldfid = NULL, sldfids, sldfide, 
                          dfid = NULL, dfids, dfide,
                          var, breaks, lwd, 
                          col, 
                          legend.pos = "bottomleft",  legend.title.txt = var, 
                          legend.title.cex = 0.8, legend.values.cex = 0.6, 
                          legend.frame = FALSE, 
                          legend.values.rnd = 0, add = TRUE){
  # test
  if((length(breaks)-1) != length(lwd)){
    stop("length(lwd) must be equal to length(breaks) - 1",call. = FALSE)
  }
  
  # joint
  if (is.null(sldfid)){
    sldf@data <- data.frame(df[match(x = paste(sldf@data[,sldfids],
                                               sldf@data[,sldfide]), 
                                     table = paste(df[,dfids], 
                                                   df[,dfide])),]) 
  } else {
    sldf@data <- data.frame(df[match(x = sldf@data[,sldfid], 
                                     table = df[,dfid]),]) 
  }
  sldf <- sldf[!is.na(sldf@data[,var]),]
  
  # lwd
  lwdMap <- lwd[findInterval(x = sldf@data[,var], vec = breaks, all.inside = TRUE)]
  
  # map
  plot(sldf, col=col,lwd = lwdMap, add = add)
  
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



