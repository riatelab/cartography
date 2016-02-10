#' @title Get Figure Dimensions
#' @description Give the dimension of a map figure to be exported in 
#' raster of vector format. \cr
#' Output dimension are based on a Spatial*DataFrame 
#' dimension ratio, the margins of the figure, a targeted width or height
#' of the figure and its resolution. 
#' @name getFigDim
#' @param spdf a Spatial*DataFrame.
#' @param width width of the figure (in pixels), either width or height 
#' must be set.
#' @param height heigth of the figure (in pixels), either width or height 
#' must be set. 
#' @param mar a numerical vector of the form c(bottom, left, top, right) 
#' which gives the number of lines of margin to be specified on the four 
#' sides of the plot (see \link{par}).
#' @param res the nominal resolution in ppi which will be recorded in the 
#' bitmap file.
#' @return A vector of width and height in pixels is returned.
#' @details The function can be used to export vector or raster files (see examples).
#' @export
#' @examples
#' \dontrun{
#' data("nuts2006")
#' spdf <- nuts0.spdf[nuts0.spdf$id=="IT",]
#' ## PNG export
#' # get figure dimension
#' sizes <- getFigDim(spdf = spdf, width = 450, mar = c(0,0,1.2,0))
#' # export the map
#' png(filename = "Italy.png", width = sizes[1], height = sizes[2])
#' par(mar = c(0,0,1.2,0))
#' plot(spdf, col  = NA, border=NA, bg = "#A6CAE0")
#' plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
#' plot(spdf, col  = "#D1914D", border="white", add=T)
#' layoutLayer(title = "Map of Italy")
#' dev.off()
#' 
#' ## PDF export
#' # get figure dimension
#' sizes <- getFigDim(spdf = spdf, width = 450, mar = c(1,1,2.2,1))
#' # export the map
#' pdf(file = "Italy.pdf", width = sizes[1]/72, height = sizes[2]/72)
#' par(mar = c(1,1,2.2,1))
#' plot(spdf, col  = NA, border=NA, bg = "#A6CAE0")
#' plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
#' plot(spdf, col  = "#D1914D", border="white", add=T)
#' layoutLayer(title = "Map of Italy")
#' dev.off()
#' }
getFigDim <- function(spdf, width = NULL, height = NULL, 
                      mar = par()$mar, res = 72){
  bb <- sp::bbox(obj = spdf)
  iw <- bb[1,2] - bb[1,1]
  ih <- bb[2,2] - bb[2,1]
  if (is.null(width) & is.null(height)){width <- 474}
  if(!is.null(width)){
    wh <- iw / ih
    widthmar <- width - ( 0.2 * (mar[2] +  mar[4]) * res )
    height <- (widthmar / wh) + ( 0.2 * (mar[1] +  mar[3]) * res )
  }else{
    hw <- ih / iw
    heightmar <- height - ( 0.2 * (mar[1] +  mar[3]) * res )
    width <- (heightmar / hw) + ( 0.2 * (mar[2] +  mar[4]) * res )
  }
  return(floor(c(width, height)))
}