#' @name propLinkLayer
#' @title Proportional Links Layer
#' @description Plot a layer of proportional links. Links widths are directly proportional to values of a variable.
#' @param x an sf object, a simple feature collection.
#' @param df a data frame that contains identifiers of starting and ending points.
#' @param xid identifier fields in x, default to the 2 first columns, character 
#' vector of length 2. (optional)
#' @param dfid identifier fields in df, default to the two first columns, character 
#' vector of length 2. (optional)
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
#' twincities.spdf <- getLinkLayer(x = nuts2.spdf, df = twincities.df[,1:2])
#' # Plot the links - Twin cities agreements between regions 
#' plot(nuts0.spdf, col = "grey60",border = "grey20")
#' propLinkLayer(x = twincities.spdf, df = twincities.df[twincities.df$fij>=5,],maxlwd = 10,
#'               legend.pos = "topright",
#'               var = "fij", 
#'               col = "#92000090", add = TRUE)
#' @export
propLinkLayer <- function(x, df, xid = NULL, dfid = NULL, 
                          var, maxlwd = 40, 
                          col, 
                          legend.pos = "bottomleft",  
                          legend.title.txt = var, 
                          legend.title.cex = 0.8, 
                          legend.values.cex = 0.6, 
                          legend.values.rnd = 0,
                          legend.frame = FALSE, 
                          add = TRUE){
  
  
  if (is.null(xid)){xid <- names(x)[1:2]}
  if (is.null(dfid)){dfid <- names(df)[1:2]}
  
  # joint
  link <- merge(x = x, y = df, by.x = xid, by.y = dfid)
  # clean
  link <- link[!is.na(link[[var]]), ]
  
  maxval <- max(link[[var]])
  link$lwd <- link[[var]] * maxlwd / maxval
  plot(st_geometry(link), lwd = link$lwd, col = col, add = add)
  
  
  if(legend.pos !="n"){
    legendPropLines(pos = legend.pos, title.txt = legend.title.txt, 
                    title.cex = legend.title.cex,
                    values.cex = legend.values.cex, var = link[[var]], 
                    lwd = link$lwd, col = col, frame = legend.frame, 
                    values.rnd = legend.values.rnd)
  }
}
