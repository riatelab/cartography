#' @name gradLinkLayer
#' @title Graduated Links Layer
#' @description Plot a layer of graduated links. Links are plotted according to discrete classes of widths.
#' @param x an sf object, a simple feature collection.
#' @param df a data frame that contains identifiers of starting and ending points and a variable.
#' @param xid identifier fields in x, character 
#' vector of length 2, default to the 2 first columns. (optional)
#' @param dfid identifier fields in df, character 
#' vector of length 2, default to the two first columns. (optional)
#' @param var name of the variable used to plot the links widths.
#' @param breaks break values in sorted order to indicate the intervals for assigning the lines widths.
#' @param lwd vector of widths (classes of widths). 
#' @param col color of the links.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
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
#' @param spdf defunct.
#' @param spdfid defunct.
#' @param spdfids defunct.
#' @param spdfide defunct.
#' @param dfids defunct.
#' @param dfide defunct.
#' @note Unlike most of cartography functions, identifiers fields are mandatory.
#' @import sp
#' @seealso \link{getLinkLayer}, \link{propLinkLayer}, \link{legendGradLines}
#' @examples
#' data("nuts2006")
#' # Create a link layer
#' twincities.sf <- getLinkLayer(x = nuts2.spdf, df = twincities.df[,1:2])
#' # Plot the links - Twin cities agreements between regions 
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' gradLinkLayer(x = twincities.sf, df = twincities.df,
#'               legend.pos = "topright",
#'               var = "fij", breaks = c(2,5,15,20,30), lwd = c(0.1,1,4,10),
#'               col = "#92000090", add = TRUE)
#' @export
gradLinkLayer <- function(x, df, xid = NULL, dfid = NULL, 
                          var, 
                          breaks = getBreaks(v = df[,var],nclass = 4,
                                             method = "quantile"), 
                          lwd = c(1,2,4,6), 
                          col = "red", 
                          legend.pos = "bottomleft",  
                          legend.title.txt = var, 
                          legend.title.cex = 0.8, 
                          legend.values.cex = 0.6, 
                          legend.values.rnd = 0,
                          legend.frame = FALSE, 
                          add = TRUE, 
                          spdf, spdfid, spdfids, spdfide, dfids, dfide){
  
  
  if(sum(c(missing(spdf), missing(spdfid), missing(spdfids), missing(spdfide), missing(dfids), missing(dfide))) != 6){
    stop("spdf, spdfid, spdfids, spdfide, dfids and dfide are defunct arguments; last used in version 1.4.2.",
         call. = FALSE)
  }
  
  
  # test
  if((length(breaks)-1) != length(lwd)){
    stop("length(lwd) must be equal to length(breaks) - 1",call. = FALSE)
  }
  
  if (is.null(xid)){xid <- names(x)[1:2]}
  if (is.null(dfid)){dfid <- names(df)[1:2]}
  
  # joint
  link <- merge(x = x, y = df, by.x = xid, by.y = dfid)
  
  # clean
  link <- link[!is.na(link[[var]]), ]
  link <- link[link[[var]] >= min(breaks) & link[[var]] <= max(breaks), ]
  
  
  # lwd
  lwdMap <- lwd[findInterval(x = link[[var]], vec = breaks, all.inside = TRUE)]
  
  # map
  plot(link, col = col, lwd = lwdMap, add = add)
  
  # legend
  legendGradLines(pos = legend.pos, title.txt = legend.title.txt, 
                  title.cex = legend.title.cex ,
                  values.cex = legend.values.cex, 
                  breaks = breaks, lwd = lwd, 
                  col = col, values.rnd = legend.values.rnd,
                  frame = legend.frame)
}



