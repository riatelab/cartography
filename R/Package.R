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
#' A \bold{vignette} contains commented scripts on how to create various maps:\cr
#' \code{vignette(topic = "cartography", package = "cartography")} \cr
#' A \bold{cheat sheet} displays a quick overview of \code{cartography}'s 
#' main features:\cr\code{vignette(topic = "cheatsheet" , package = "cartography")}
#' 
#'  
#' @section Symbology:
#' These functions build cartographic layers.
#' \itemize{
#' \item{Proportional symbols layer (\link{propSymbolsLayer}, \link{propSymbolsChoroLayer}, \link{propSymbolsTypoLayer}, \link{propTrianglesLayer})}
#' \item{Choropleth layer (\link{choroLayer})}
#' \item{Typology layer (\link{typoLayer})}
#' \item{Flows layer (\link{propLinkLayer}, \link{gradLinkLayer}, \link{gradLinkTypoLayer})}
#' \item{Discontinuities layer (\link{discLayer})}
#' \item{Access to cartographic OpenStreetMap APIs (\link{tilesLayer})}
#' \item{Dot density layer (\link{tilesLayer})}
#' \item{Labels layer (\link{labelLayer})}
#' \item{Smooth layer (\link{smoothLayer})}
#' \item{png layer (\link{pngLayer})}
#' \item{Hatched/pattern layer (\link{hatchedLayer})}
#' }
#' 
#' @section Transformations:
#' These functions transform or create spatial objects.
#' \itemize{
#' \item{Extract polygons borders (\link{getBorders}, \link{getOuterBorders})}
#' \item{Build a regular grid layer (\link{getGridLayer})}
#' \item{Create a links layer from a data.frame of links (\link{getLinkLayer})}
#' \item{Create a png layer (\link{getPngLayer})}
#' \item{Create a pencil layer (\link{getPencilLayer})}
#' \item{Get tiles from OpenStreetMap servers (\link{getTiles})}
#' }
#'  
#' @section Map Layout:
#' These functions are dedicated to the map layout design.
#' \itemize{
#' \item{Scale bar (\link{barscale})}
#' \item{Get figure dimensions (\link{getFigDim})}
#' \item{Layout layer (\link{layoutLayer})}
#' \item{North arrow (\link{north})}
#' }
#' 
#' @section Color Palettes:
#' Use these function to build custom color palettes.
#' \itemize{
#' \item{Build cartographic palettes (\link{carto.pal})}
#' }
#' 
#' @section Legends:
#' These functions create legends.
#' \itemize{
#' \item{Legends (\link{legendBarsSymbols}, \link{legendChoro}, \link{legendCirclesSymbols}, \link{legendGradLines}, \link{legendPropLines}, \link{legendPropTriangles}, \link{legendSquaresSymbols}, \link{legendTypo}, \link{legendHatched})}
#' }
#' 
#' @section Classification:
#' This function uses classification methods for binning data.
#' \itemize{
#' \item{Classification (\link{getBreaks})}
#' }
#' 
#' @keywords internal
#' @docType package
NULL
