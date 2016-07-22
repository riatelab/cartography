#' @title Smooth Layer
#' @name smoothLayer
#' @description Plot a layer of smoothed data. It can also compute a ratio of potentials.\cr \cr
#' This function is a wrapper around the \code{\link[SpatialPosition:quickStewart]{quickStewart}} function in 
#' \code{\link[SpatialPosition]{SpatialPosition}} package.\cr \cr
#' The SpatialPosition package also provides: \itemize{
#' \item{vignettes to explain the computation of potentials;} 
#' \item{more customizable inputs and outputs (custom distance matrix, raster output...);}
#' \item{other functions related to spatial interactions (Reilly  and Huff catchment areas).}}
#' @param spdf a SpatialPolygonsDataFrame.
#' @param df a data frame that contains the values to compute If df is missing 
#' spdf@data is used instead. 
#' @param spdfid name of the identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid name of the identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric field in df used to compute potentials.
#' @param var2 name of the numeric field in df used to compute potentials. 
#' This field is used for ratio computation (see Details).
#' @param typefct character; spatial interaction function. Options are "pareto" 
#' (means power law) or "exponential".
#' If "pareto" the interaction is defined as: (1 + alpha * mDistance) ^ (-beta).
#' If "exponential" the interaction is defined as: 
#' exp(- alpha * mDistance ^ beta).
#' The alpha parameter is computed from parameters given by the user 
#' (\code{beta} and \code{span}).
#' @param span numeric; distance where the density of probability of the spatial 
#' interaction function equals 0.5.
#' @param beta numeric; impedance factor for the spatial interaction function.  
#' @param resolution numeric; resolution of the output SpatialPointsDataFrame
#'  (in map units). 
#' @param nclass	numeric; a targeted number of classes (default to 8). Not used if breaks is set.
#' @param breaks numeric; a vector of values used to discretize the potentials. 
#' @param mask SpatialPolygonsDataFrame; mask used to clip contours of potentials.
#' @param col a vector of colors. Note that if breaks is specified there must be one less 
#' colors specified than the number of break. 
#' @param border color of the polygons borders.
#' @param lwd borders width.
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
#' @details 
#' If var2 is provided the ratio between the potentials of var (numerator) 
#' and var2 (denominator) is computed.
#' @return An \code{\link{invisible}} SpatialPolygonsDataFrame is returned (see \code{\link[SpatialPosition:quickStewart]{quickStewart}}).
#' @export
#' @seealso \link[SpatialPosition]{quickStewart}, \link[SpatialPosition]{SpatialPosition}, \link{choroLayer}
#' @examples
#' \dontrun{
#' data("nuts2006")
#' 
#' # Potential of GDP
#' smoothLayer(spdf = nuts3.spdf, df = nuts3.df, 
#'             var = 'gdppps2008',
#'             span = 75000, beta = 2, 
#'             mask = nuts0.spdf, 
#'             legend.title.txt = "GDP", 
#'             legend.pos = "topright", legend.values.rnd = -2)  
#' 
#' # Potential of GDP per Capita
#' nuts3.df$gdppps2008 <- nuts3.df$gdppps2008 * 1000000
#' smoothLayer(spdf = nuts3.spdf, df = nuts3.df, 
#'             var = 'gdppps2008', var2 = 'pop2008',
#'             span = 75000, beta = 2, 
#'             mask = nuts0.spdf, 
#'             legend.title.txt = "GDP PER CAPITA", 
#'             legend.pos = "topright", legend.values.rnd = -2)  
#'}             
smoothLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, 
                        var, 
                        var2 = NULL, 
                        typefct = "exponential", 
                        span, 
                        beta, 
                        resolution = NULL, 
                        mask = NULL, 
                        nclass = 8,
                        breaks = NULL, 
                        col = NULL,
                        border = "grey20", lwd = 1,
                        legend.pos = "bottomleft", 
                        legend.title.txt = "Potential",
                        legend.title.cex = 0.8, 
                        legend.values.cex = 0.6,
                        legend.values.rnd = 0,
                        legend.frame = FALSE,
                        add = FALSE){
  if (!requireNamespace("SpatialPosition", quietly = TRUE)) {
    stop("'SpatialPosition' package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!'package:SpatialPosition' %in% search()){
    attachNamespace('SpatialPosition')
  }
  if (missing(df)){df <- spdf@data}
  # Potential
  pot.spdf <- SpatialPosition::quickStewart(spdf = spdf, df = df, 
                                            spdfid = spdfid, dfid = dfid, 
                                            var = var, var2 = var2, 
                                            typefct = typefct,
                                            span = span, 
                                            resolution = resolution, 
                                            beta = beta, mask = mask, 
                                            nclass=nclass,
                                            breaks = breaks)
  # breaks
  if(is.null(breaks)){
    breaks <- sort(c(unique(pot.spdf$min), max(pot.spdf$max)))
  }
  # map
  choroLayer(spdf = pot.spdf, df = pot.spdf@data, var = "center", 
             breaks = breaks, col = col, border = border, lwd = lwd, 
             legend.pos = legend.pos, legend.title.txt = legend.title.txt, 
             legend.title.cex = legend.title.cex, 
             legend.values.cex = legend.values.cex,
             legend.values.rnd = legend.values.rnd, 
             legend.frame = legend.frame, add = add)
  return(invisible(pot.spdf))
}
