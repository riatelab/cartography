#' @title Cartography Package
#' @name cartography
#' @rdname cartography
#' @description 
#' This package helps to design cartographic representations such as 
#' proportional symbols, choropleth, typology, flows or discontinuities maps. 
#' It also offers several features that improve the graphic presentation of 
#' maps, for instance, map palettes, layout elements (scale, north arrow, 
#' title...), labels or legends.
#' 
#' A \bold{vignette} contains commented scripts on how to create various maps 
#' and a \bold{cheat sheet} displays a quick overview of \code{cartography}'s 
#' main features:\cr  
#' - \code{vignette(topic = "cartography", package = "cartography")};\cr
#' - \code{vignette(topic = "cheatsheet" , package = "cartography")}.\cr
#' 
#'  
#' 
#' Main functions : 
#' \itemize{
#' \item{Proportional symbols maps (circles, squares, bars)\cr 
#' \link{propSymbolsLayer}, \link{propSymbolsChoroLayer}, 
#' \link{propSymbolsTypoLayer}, \link{propTrianglesLayer}}
#' 
#' \item{Choropleth maps (main classification methods are available)\cr
#' \link{choroLayer}}
#' 
#' \item{Typology maps\cr
#' \link{typoLayer}}
#' 
#' \item{Flow maps (proportional and classified links)\cr
#' \link{getLinkLayer}, \link{propLinkLayer}, \link{gradLinkLayer}, \link{gradLinkTypoLayer}}
#' 
#' \item{Discontinuities maps\cr
#' \link{getBorders}, \link{discLayer}}
#' 
#' \item{Cartographic palettes\cr
#' \link{carto.pal}}
#' 
#' \item{Layout (scale, north arrow, title...)\cr
#' \link{layoutLayer}, \link{north}, \link{barscale}}
#' 
#' \item{Labels\cr
#' \link{labelLayer}}
#' 
#' \item{Legends \cr
#' \link{legendBarsSymbols},
#' \link{legendChoro}, \link{legendCirclesSymbols}, \link{legendGradLines}, 
#' \link{legendPropLines}, \link{legendPropTriangles}, 
#' \link{legendSquaresSymbols}, \link{legendTypo}}
#' 
#' \item{Access to cartographic APIs (via rosm package)\cr
#' \link{getTiles}, \link{tilesLayer}}
#' 
#' \item{Irregular polygons to regular grid, transformation with data handling\cr
#' \link{getGridLayer}}
#' 
#' }
#' 
#' 
#' @docType package
NULL
