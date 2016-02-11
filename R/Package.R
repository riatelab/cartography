#' @title Cartography Package
#' @name cartography
#' @description 
#' The cartography package proposes thematic mapping functions.
#' It allows various cartographic representation:  
#' \itemize{
#' \item{Proportional symbols maps (circles, squares, bars)\cr 
#' \link{propSymbolsLayer}, \link{propSymbolsChoroLayer}, 
#' \link{propSymbolsTypoLayer}, \link{propTrianglesLayer}}
#' \item{Chroropleth maps (main discretization methods are available)\cr
#' \link{choroLayer}}
#' \item{Typology maps\cr
#' \link{typoLayer}}
#' \item{Flow maps (proportional and classified links)\cr
#' \link{getLinkLayer}, \link{propLinkLayer}, \link{gradLinkLayer}}
#' \item{Discontinuities maps (variable size and color of borders)\cr
#' \link{getBorders}, \link{discLayer}}
#' \item{...}
#' }
#' 
#' It also proposes some additional useful features like:
#' \itemize{
#' \item{Cartographic palettes (palettes adapted to cartographic representation)\cr
#' \link{carto.pal}}
#' \item{Layout (scale, north arrow, title...)\cr
#' \link{layoutLayer}}
#' \item{Labels\cr
#' \link{labelLayer}}
#' \item{Nice legends \cr
#' \link{legendBarsSymbols},
#' \link{legendChoro}, \link{legendCirclesSymbols}, \link{legendGradLines}, 
#' \link{legendPropLines}, \link{legendPropTriangles}, 
#' \link{legendSquaresSymbols}, \link{legendTypo}
#' }
#' \item{Access to cartographic API (via rosm package)\cr
#' \link{getTiles}, \link{tilesLayer}
#' }
#' \item{Irregular polygons to regular grid transformation with data handling\cr
#' \link{getGridLayer}, \link{getGridData}}
#' \item{...}
#' }
#' 
#' Functions starting with "get" build R objects.\cr
#' Functions ending with "Layer" plot cartographic layers.\cr
#' Functions starting with "legend" plot legends.\cr
#' 
#' The vignette contains commented scripts on how to build various types of maps 
#' with \code{cartography}: 
#' \code{vignette(topic = "cartography")}
#' 
#' @docType package
NULL
