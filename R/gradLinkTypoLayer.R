#' @name gradLinkTypoLayer
#' @title Graduated and Colored Links Layer
#' @description Plot a layer of colored and graduated links. 
#' Links are plotted according to discrete classes of widths. 
#' Colors depend on a discrete variable of categories.
#' @param x an sf object, a simple feature collection.
#' @param df a data frame that contains identifiers of starting and ending points and variables.
#' @param xid identifier fields in x, character 
#' vector of length 2, default to the 2 first columns. (optional)
#' @param dfid identifier fields in df, character 
#' vector of length 2, default to the two first columns. (optional)
#' @param var name of the variable used to plot the links widths.
#' @param var2 name of the variable used to plot the links colors.
#' @param breaks break values in sorted order to indicate the intervals for assigning the lines widths.
#' @param lwd vector of widths (classes of widths). 
#' @param col color of the links.
#' @param legend.var.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)).
#' @param legend.var2.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)).
#' @param legend.var.title.txt title of the legend (numeric data).
#' @param legend.var2.title.txt title of the legend (factor data).
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.var.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.var2.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.var2.nodata text for "no data" values
#' @param legend.var2.values.order values order in the legend, a character vector 
#' that matches var modalities. Colors will be affected following this order. 
#' @param colNA no data color. 
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @param spdf defunct.
#' @param spdfid defunct.
#' @param spdfids defunct.
#' @param spdfide defunct.
#' @param dfids defunct.
#' @param dfide defunct.
#' @note Unlike most of cartography functions, identifiers fields are mandatory.
#' @seealso \link{getLinkLayer}, \link{propLinkLayer}, \link{legendGradLines}, \link{gradLinkLayer}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' mob <- read.csv(system.file("csv/mob.csv", package="cartography"))
#' # Create a link layer - work mobilities to Fort-de-France (97209) and 
#' # Le Lamentin (97213)
#' mob.sf <- getLinkLayer(x = mtq, df = mob[mob$j %in% c(97209, 97213),], 
#'                        dfid = c("i", "j"))
#' # Plot the links - Work mobility
#' plot(st_geometry(mtq), col = "grey60",border = "grey20")
#' gradLinkTypoLayer(x = mob.sf, df = mob,
#'                   var = "fij", 
#'                   breaks = c(109,500,1000,2000,4679), 
#'                   lwd = c(1,2,4,10),
#'                   var2='j', add = TRUE)
#' @export
gradLinkTypoLayer <- function(x, df, xid = NULL, dfid = NULL,
                              var, 
                              breaks = getBreaks(v = df[,var],nclass = 4,
                                                 method = "quantile"), 
                              lwd = c(1,2,4,6),
                              var2,
                              col = NULL, colNA = "white",
                              legend.title.cex = 0.8, 
                              legend.values.cex = 0.6, 
                              legend.values.rnd = 0,
                              legend.var.pos = "bottomleft",
                              legend.var.title.txt = var,
                              legend.var.frame = FALSE,
                              legend.var2.pos = "topright", 
                              legend.var2.title.txt = var2,
                              legend.var2.values.order = NULL,
                              legend.var2.nodata = "no data",
                              legend.var2.frame = FALSE,
                              add = TRUE, 
                              spdf, spdfid, spdfids, spdfide, dfids, dfide){
  if(sum(c(missing(spdf), missing(spdfid), missing(spdfids), missing(spdfide), 
           missing(dfids), missing(dfide))) != 6){
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
  link <- merge(x = x[,xid], y = df, by.x = xid, by.y = dfid)
  
  # clean 
  link <- link[!is.na(link[[var]]), ]
  link <- link[link[[var]] >= min(breaks) & link[[var]] <= max(breaks), ]
  link <- link[order(link[[var]], decreasing = TRUE),]
  
  # lwd
  lwdMap <- lwd[findInterval(x = link[[var]], vec = breaks, all.inside = TRUE)]
  
  mod <- unique(link[[var2]])
  mod <- mod[!is.na(mod)]
  
  # check nb col vs nb mod
  col <- checkCol(col, mod)
  
  # check legend.var2.values.order vs mod values
  legend.var2.values.order <- checkOrder(legend.var2.values.order, mod)
  
  # get the colors 
  refcol <- data.frame(mod = legend.var2.values.order, 
                       col = col[1:length(legend.var2.values.order)], 
                       stringsAsFactors = FALSE)
  mycols <- refcol[match(link[[var2]], refcol[,1]),2]
  
  nodata <- FALSE
  if(max(is.na(link[[var2]])>0)){
    nodata <- TRUE
    mycols[is.na(mycols)] <- colNA
  }
  
  # map
  plot(sf::st_geometry(link), col = mycols ,lwd = lwdMap, add = add)
  
  # legend links
  legendGradLines(pos = legend.var.pos, 
                  title.txt = legend.var.title.txt, 
                  title.cex = legend.title.cex ,
                  values.cex = legend.values.cex, 
                  breaks = breaks, lwd = lwd, 
                  col = "grey20", 
                  values.rnd = legend.values.rnd,
                  frame = legend.var.frame)
  # legend typo
  legendTypo(pos = legend.var2.pos, 
             title.txt = legend.var2.title.txt,
             title.cex = legend.title.cex, 
             values.cex = legend.values.cex,
             categ = refcol[,1], 
             col = refcol[,2], 
             frame = legend.var2.frame, 
             symbol="line", 
             nodata = nodata,nodata.col = colNA, 
             nodata.txt = legend.var2.nodata)
}



