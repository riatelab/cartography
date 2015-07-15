#' @title Discontinuities Layer
#' @description This function computes and plots spatial discontinuities. The 
#' discontinuities are plotted over the layer outputed by the \link{getBorders} function.
#' The line widths reflect the ratio between values of an indicator in neighbouring units.
#' @name discLayer
#' @param spdf SpatialLinesDataFrame; as outputed by the \link{getBorders} function.
#' @param df data.frame; df contains the values.
#' @param spdfid1 character; first id of the border, default to the second column 
#' of the spdf data.frame. (optional)
#' @param spdfid2 character; second id of the border, default to the third column 
#' of the spdf data.frame. (optional)
#' @param dfid character; id field in df, default to the first column 
#' of df. (optional)
#' @param var character; name of the numeric field in df used to compute and plot discontinuities.
#' @param col character; color of lines.
#' @param nclass numeric; a targeted number of classes. If null, the number of 
#' class is automatically defined (see Details).
#' @param method character; a discretization method; one of "sd", "equal", 
#' "quantile", "jenks","q6","geom"  (see Details).
#' @param threshold numeric; share of represented borders, value between 0 
#' (nothing) and 1 (all the discontinuities).
#' @param sizemin numeric; thickness of the smaller line.
#' @param sizemax numeric; thickness of the bigger line.
#' @param type character; "rel" or "abs", type of discontinuity measure (see Details).
#' @param legend.pos character; position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt character; title of the legend.
#' @param legend.title.cex numeric; size of the legend title.
#' @param legend.values.cex numeric; size of the values in the legend.
#' @param legend.values.rnd numeric; number of decimal places of the values in 
#' the legend.
#' @param legend.frame boolean; whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add boolean; whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @details 
#' "sd", "equal", "quantile" and "jenks" are \link{classIntervals} methods. The "q6" method
#' uses the following \link{quantile} probabilities: 0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.   
#' The "geom" method consists is based on a geometric progression along the variable values.  
#' 
#' The "rel" type of discontinuity is the result of pmax(value unit 1 / value unit 2, value unit 2 / value unit 1).
#' The "abs" type of discontinuity is the result of pmax(value unit 1 - value unit 2, value unit 2 - value unit 1).
#' 
#' @examples
#' data(nuts2006)
#' # Get borders
#' nuts0.contig.spdf <- getBorders(nuts0.spdf)
#' # GDP per capita
#' nuts0.df$gdpcap <- nuts0.df$gdppps2008/nuts0.df$pop2008
#' 
#' plot(nuts0.spdf, col="#CCCCCC", lwd=1, border="white")
#' discLayer(spdf = nuts0.contig.spdf, df = nuts0.df,
#'           var = "gdpcap", col="red", nclass=5,  
#'           method="quantile", threshold = 0.5, sizemin = 1,
#'           sizemax = 10, type = "rel", legend.frame = TRUE,
#'           legend.title.txt = "GDP per Capita discontinuities\n(relative)", 
#'           legend.pos = "topright",
#'           add=TRUE)
#' @export
discLayer <- function(spdf, df, spdfid1 = NULL, spdfid2=NULL, dfid=NULL,
                      var, col = "red", nclass=NULL,
                      method="quantile", threshold = 0.75, sizemin = 1,
                      sizemax = 10, type = "rel",
                      legend.title.txt="legend title", legend.pos = "bottomleft", 
                      legend.title.cex = 0.8, legend.values.cex = 0.6, 
                      legend.values.rnd = 2,legend.frame=FALSE,add = FALSE){
  
  
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
}

