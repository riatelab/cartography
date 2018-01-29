#' @title Discontinuities Layer
#' @description This function computes and plots spatial discontinuities. The 
#' discontinuities are plotted over the layer outputed by the \link{getBorders} function.
#' The line widths reflect the ratio or the difference between values of an indicator 
#' in two neighbouring units.
#' @name discLayer
#' @param x an sf object, a simple feature collection, as outputed by the \link{getBorders} function. 
#' @param df a data frame that contains the values used to compute and plot discontinuities.
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in df used to compute and plot discontinuities.
#' @param col color of the discontinuities lines.
#' @param nclass a targeted number of classes. If null, the number of 
#' class is automatically defined (see \link{getBreaks}).
#' @param method a discretization method; one of "sd", "equal", "quantile", "fisher-jenks","
#' q6", "geom", "arith", "em" or "msd" (see \link{getBreaks}).
#' @param threshold share of represented borders, value between 0 
#' (nothing) and 1 (all the discontinuities).
#' @param sizemin thickness of the smallest line.
#' @param sizemax thickness of the biggest line.
#' @param type type of discontinuity measure, one of "rel" or "abs" (see Details).
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values in 
#' the legend.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @param spdf defunct.
#' @param spdfid1 defunct.
#' @param spdfid2 defunct.
#' @return  An \link{invisible} sf object (MULTISTRING) with the discontinuity measures is returned. 
#' @details 
#' The "rel" type of discontinuity is the result of pmax(value unit 1 / value unit 2, value unit 2 / value unit 1).\cr
#' The "abs" type of discontinuity is the result of pmax(value unit 1 - value unit 2, value unit 2 - value unit 1).
#' @seealso \link{getBorders}, \link{gradLinkLayer}, \link{legendGradLines}
#' @examples
#' library(sp)
#' data(nuts2006)
#' # Get borders
#' nuts0.contig <- getBorders(x = nuts0.spdf)
#' # GDP per capita
#' nuts0.df$gdpcap <- nuts0.df$gdppps2008/nuts0.df$pop2008
#' # Plot countries
#' plot(nuts0.spdf, col="#CCCCCC", lwd=1, border="white")
#' # Plot discontinuities
#' discLayer(x = nuts0.contig, df = nuts0.df,
#'           var = "gdpcap", col="red", nclass=5,
#'           method="quantile", threshold = 0.5, sizemin = 1,
#'           sizemax = 10, type = "rel", legend.frame = TRUE,
#'           legend.title.txt = "GDP per Capita discontinuities\n(relative)",
#'           legend.pos = "topright", add=TRUE)
#' 
#' library(sf)
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' # Get borders
#' mtq.borders <- getBorders(x = mtq)
#' # Employees share in the pop
#' mtq$emp_share <- 100 * mtq$C13_CS5/mtq$C13_POP
#' # Plot this share
#' choroLayer(x = mtq, var = "emp_share", border = NA, method = 'q6', 
#'            legend.values.rnd = 1, legend.pos = "topleft", 
#'            legend.title.txt = "Share of employees\nin the population\n(age > 15 y.o.)" )
#' # Plot discontinuities
#' discLayer(x = mtq.borders, df = mtq,
#'           var = "emp_share", col="darkorange2", nclass=3,
#'           method="quantile", threshold = 0.5, sizemin = 0.5,
#'           sizemax = 10, type = "abs", 
#'           legend.title.txt = "Discontinuities\n(absolute difference)",
#'           legend.pos = "bottomleft", add=TRUE)
#' @export
discLayer <- function(x, df, dfid = NULL, var, 
                      method="quantile", nclass = 4, threshold = 0.75, 
                      type = "rel",
                      sizemin = 1, sizemax = 10,
                      col = "red", 
                      legend.pos = "bottomleft",
                      legend.title.txt="legend title",  
                      legend.title.cex = 0.8, 
                      legend.values.cex = 0.6, 
                      legend.values.rnd = 2,
                      legend.frame=FALSE,
                      add = TRUE, spdf, spdfid1, spdfid2){
  
  if(sum(c(missing(spdf), missing(spdfid1), missing(spdfid2))) != 3){
    stop("spdf, spdfid1 and spdfid2 are defunct arguments; last used in version 1.4.2.",
         call. = FALSE)
  }
  if (is.null(dfid)){dfid <- names(df)[1]}
  df <- data.frame(df)
  # Join (1 and 2)
  x <- merge(x, df[,c(dfid, var)], by.x = names(x)[3], by.y = dfid, all.x = TRUE)
  x <- merge(x, df[,c(dfid, var)], by.x = names(x)[3], by.y = dfid, all.x = TRUE)
  names(x)[4:5] <- c('var1', 'var2')
  
  # elimination des valeurs manquantes
  x <- x[!is.na(x$var1) & !is.na(x$var2), ]
  
  # discontinuité relative ou absolue
  if (type == "rel") {x$disc <- pmax(x$var1/x$var2, x$var2/x$var1)}
  if (type == "abs") {x$disc <- pmax(x$var1-x$var2, x$var2-x$var1)}
  
  x.out <- x
  colnames(x.out)[4:5] <- c(paste(var, 1, sep=""), paste(var, 2, sep=""))
  
  # Valeur minimal
  minvar <- as.numeric(quantile(x$disc,probs = c(1-threshold)))
  
  # Discretisation
  x <- x[x$disc >= minvar,]
  distr <- getBreaks(v = x$disc, nclass = nclass, method = method)
  
  # Classes de tailles
  ete <- (sizemax-sizemin) / nclass
  sizes <- sizemin
  for(i in 1:nclass){sizes <- c(sizes,sizes[i]+ete)}
  
  # Affectation des tailles au spdf
  x$sizesMap <- sizes[(findInterval(x$disc,distr,all.inside=TRUE))]
  
  # Cartographie
  plot(st_geometry(x), col = col, lwd = x$sizesMap, add = add)
  
  # Legend
  legendGradLines(pos = legend.pos, title.txt = legend.title.txt, 
                  title.cex = legend.title.cex ,
                  values.cex = legend.values.cex, 
                  breaks = distr, lwd = sizes, 
                  col = col, values.rnd = legend.values.rnd,
                  frame = legend.frame)
  
  invisible(x.out)
}
