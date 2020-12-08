#' @title Get Figure Dimensions
#' @description Give the dimension of a map figure to be exported in 
#' raster or vector format. \cr
#' Output dimension are based on a spatial object dimension ratio, margins of 
#' the figure, a targeted width or height and a resolution. 
#' @name getFigDim
#' @param x an sf object, a simple feature collection or a Spatial*DataFrame. 
#' @param spdf defunct.
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
#' @keywords internal
#' @seealso \link{tc_init}
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' 
#' ## PNG export
#' # get figure dimension
#' sizes <- getFigDim(x = mtq, width = 450, mar = c(0,0,1.2,0))
#' # export the map
#' png(filename = "mtq.png", width = sizes[1], height = sizes[2])
#' par(mar = c(0,0,1.2,0))
#' plot(st_geometry(mtq), col = "#D1914D", border = "white", bg = "#A6CAE0")
#' title("Madinina")
#' dev.off()
#' 
#' ## PDF export
#' # get figure dimension
#' sizes <- getFigDim(x = mtq, width = 450, mar = c(1,1,2.2,1))
#' # export the map
#' pdf(file = "mtq.pdf", width = sizes[1]/72, height = sizes[2]/72)
#' par(mar = c(1,1,2.2,1))
#' plot(st_geometry(mtq), col = "#D1914D", border = "white", bg = "#A6CAE0")
#' title("Madinina")
#' dev.off()
#' }
getFigDim <- function(x, spdf, width = NULL, height = NULL, 
                      mar = par('mar'), res = 72){
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::getFigDim()",
                            with = "cartography::tc_init()")  
  if(!missing(spdf)){
    stop("spdf is defunct; use x instead.", call. = FALSE)
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
