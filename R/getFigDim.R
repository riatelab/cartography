#' @title Get Figure Dimensions
#' @description Give the dimension of a map figure to be exported in 
#' raster or vector format. \cr
#' Output dimension are based on a spatial object dimension ratio, margins of 
#' the figure, a targeted width or height and a resolution. 
#' @name getFigDim
#' @param x an sf object, a simple feature collection or a Spatial*DataFrame. 
#' @param spdf deprecated, a Spatial*DataFrame.
#' @param width width of the figure (in pixels), either width or height 
#' must be set.
#' @param height height of the figure (in pixels), either width or height 
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
#' library(sp)
#' data("nuts2006")
#' italy <- nuts0.spdf[nuts0.spdf$id=="IT",]
#' 
#' ## PNG export
#' # get figure dimension
#' sizes <- getFigDim(x = italy, width = 450, mar = c(0,0,1.2,0))
#' # export the map
#' png(filename = "Italy.png", width = sizes[1], height = sizes[2])
#' par(mar = c(0,0,1.2,0))
#' plot(italy, col = NA, border=NA, bg = "#A6CAE0")
#' plot(world.spdf, col = "#E3DEBF", border = NA, add = TRUE)
#' plot(italy, col = "#D1914D", border = "white", add = TRUE)
#' layoutLayer(title = "Map of Italy")
#' dev.off()
#' 
#' ## PDF export
#' # get figure dimension
#' library(sf)
#' mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
#' sizes <- getFigDim(x = mtq, width = 450, mar = c(1,1,2.2,1))
#' # export the map
#' pdf(file = "Martinique.pdf", width = sizes[1]/72, height = sizes[2]/72)
#' par(mar = c(1,1,2.2,1))
#' plot(st_geometry(mtq), col = "#D1914D", border = "white", bg = "#A6CAE0")
#' layoutLayer(title = "Map of Martinique")
#' dev.off()
#' }
getFigDim <- function(x, spdf, width = NULL, height = NULL, 
                      mar = par('mar'), res = 72){
  
  if(!missing(spdf)){
    warning("spdf is deprecated; use x instead.", call. = FALSE)
  }
  
  if(missing(x)){
    x <- sf::st_as_sf(spdf)
  }
  
  if(methods::is(x, "Spatial")){
    x <- sf::st_as_sf(x)
  }
  
  
  bb <- sf::st_bbox(x)
  iw <- bb[3] - bb[1]
  ih <- bb[4] - bb[2]
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
  return(unname(floor(c(width, height))))
}
