#' @title Discontinuities Layer
#' @description This function computes and plots spatial discontinuities. The 
#' discontinuities are plotted over the layer outputed by the \link{getBorders} function.
#' The line widths reflect the ratio between values of an indicator in two neighbouring units.
#' @name discLayer
#' @param spdf a SpatialLinesDataFrame, as outputed by the \link{getBorders} function.
#' @param df a data frame that contains the values used to compute and plot discontinuities.
#' @param spdfid1 first identifier of the border, default to the second column 
#' of the spdf data frame. (optional)
#' @param spdfid2 second identifier of the border, default to the third column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in df used to compute and plot discontinuities.
#' @param col color of the discontinuities lines.
#' @param nclass a targeted number of classes. If null, the number of 
#' class is automatically defined (see \link{discretization} Details).
#' @param method a discretization method; one of "sd", "equal", 
#' "quantile", "jenks","q6" or "geom"  (see \link{discretization} Details).
#' @param threshold share of represented borders, value between 0 
#' (nothing) and 1 (all the discontinuities).
#' @param sizemin thickness of the smallest line.
#' @param sizemax thickness of the biggest line.
#' @param type type of discontinuity measure, one of "rel" or "abs" (see Details).
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
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
#' @return  An invisible (\link{invisible}) SpatialLinesDataFrame with the discontinuity measures is returned. 
#' @details 
#' The "rel" type of discontinuity is the result of pmax(value unit 1 / value unit 2, value unit 2 / value unit 1).\cr
#' The "abs" type of discontinuity is the result of pmax(value unit 1 - value unit 2, value unit 2 - value unit 1).
#' @seealso \link{getBorders}, \link{gradLinkLayer}, \link{legendGradLines}
#' @examples
#' data(nuts2006)
#' # Get borders
#' nuts0.contig.spdf <- getBorders(nuts0.spdf)
#' # GDP per capita
#' nuts0.df$gdpcap <- nuts0.df$gdppps2008/nuts0.df$pop2008
#' # Plot countries
#' plot(nuts0.spdf, col="#CCCCCC", lwd=1, border="white")
#' # Plot discontinuities
#' discLayer(spdf = nuts0.contig.spdf, df = nuts0.df,
#'           var = "gdpcap", col="red", nclass=5,
#'           method="quantile", threshold = 0.5, sizemin = 1,
#'           sizemax = 10, type = "rel", legend.frame = TRUE,
#'           legend.title.txt = "GDP per Capita discontinuities\n(relative)",
#'           legend.pos = "topright", add=TRUE)
#' @export
discLayer <- function(spdf, df, spdfid1 = NULL, spdfid2=NULL, dfid=NULL, var, 
                      method="quantile", nclass=NULL, threshold = 0.75, 
                      type = "rel",
                      sizemin = 1, sizemax = 10,
                      col = "red", 
                      legend.pos = "bottomleft",
                      legend.title.txt="legend title",  
                      legend.title.cex = 0.8, 
                      legend.values.cex = 0.6, 
                      legend.values.rnd = 2,
                      legend.frame=FALSE,
                      add = TRUE){
  
  
  if (is.null(spdfid1)){spdfid1 <- names(spdf@data)[2]}
  if (is.null(spdfid2)){spdfid2 <- names(spdf@data)[3]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  # Join (1 and 2)
  spdf@data <- data.frame(spdf@data, var1=df[match(spdf@data[,spdfid1], 
                                                   df[,dfid]),var])
  spdf@data <- data.frame(spdf@data, var2=df[match(spdf@data[,spdfid2], 
                                                   df[,dfid]),var])
  
  
  # elimination des valeurs manquantes
  spdf <- spdf[!is.na(spdf@data$var1),]
  spdf <- spdf[!is.na(spdf@data$var2),]
  
  # discontinuité relative ou absolue
  if (type == "rel") {spdf@data$disc <- pmax(spdf@data$var1/spdf@data$var2,
                                             spdf@data$var2/spdf@data$var1)}
  if (type == "abs") {spdf@data$disc <- pmax(spdf@data$var1-spdf@data$var2,
                                             spdf@data$var2-spdf@data$var1)}
  
  spdf.out <- spdf
  colnames(spdf.out@data)[4:5] <- c(paste(var, 1, sep=""), paste(var, 2, sep=""))
  # Valeur muinimal
  minvar <- as.numeric(quantile(spdf@data$disc,probs = c(1-threshold)))
  
  # Discretisation
  spdf <- spdf[spdf@data$disc >= minvar,]
  distr <- discretization(v = spdf@data$disc, nclass = nclass, method = method)
  
  # Classes de tailles
  x <- (sizemax-sizemin)/nclass
  sizes <- sizemin
  for(i in 1:nclass){sizes <- c(sizes,sizes[i]+x)}
  
  # Affectation des tailles au spdf
  sizesMap <- sizes[(findInterval(spdf@data$disc,distr,all.inside=TRUE))]
  spdf@data <- data.frame(spdf@data,sizesMap)
  
  # Cartographie
  plot(spdf, col=col,lwd = spdf@data$sizesMap, add = add)
  
  # Legend
  if(legend.pos !="n"){
    legendGradLines(pos = legend.pos, title.txt = legend.title.txt, 
                    title.cex = legend.title.cex ,
                    values.cex = legend.values.cex, 
                    breaks = distr, lwd = sizes, 
                    col = col, values.rnd = legend.values.rnd,
                    frame = legend.frame)
  }
  
  invisible(spdf.out)
}
