#' @title Scale Bar
#' @description Plot a scale bar. 
#' @name barscale
#' @param size size of the scale bar in units (default to km). If size is not 
#' set, an automatic size is used (1/10 of the map width).
#' @param lwd width of the scale bar.
#' @param cex cex of the text.
#' @param pos position of the legend, default to "bottomright". 
#' "bottomright" or a vector of two coordinates (c(x, y)) are possible.
#' @param style style of the legend, either "pretty" or "oldschool". The 
#' "oldschool" style only uses the "size" parameter.   
#' @param unit units used for the scale bar. Can be "mi" for miles, 
#' "m" for meters, or "km" for kilometers (default)
#' @note This scale bar is not accurate on unprojected (long/lat) maps.
#' @export
#' @seealso \link{layoutLayer}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq), col = "grey60", border = "grey20")
#' barscale(size = 5)
#' barscale(size = 5, lwd = 2, cex = .9, pos = c(714000, 1596000))
barscale <- function(size, lwd = 1.5, cex = 0.6, 
                     pos = "bottomright", style = "pretty", unit = "km"){
  # size = 10
  mapExtent <- par()$usr
  x <- mapExtent[1:2]
  y <- mapExtent[3:4]
  inset <- min(diff(x), diff(y)) / 40
  
  # default scale
  if(missing(size) || is.null(size)){
    size <- diff(x)/10
    size <- unit_conversion(size = size, unit_in = "m", unit_out = unit)
    size_text <- signif(size, digits = 0)
    size <- unit_conversion(size = size_text, unit_in = unit, unit_out = "m")
  }else{
    # convert distance into meters based on dist_unit
    size_text <- as.character(size)
    size <- unit_conversion(size, unit_in = unit, unit_out = "m")
  }
  
  # label
  labelscale <- paste(size_text, unit, sep =" ")
  
  # xy pos
  xscale <- x[2] - inset * 0.5 - size
  yscale <- y[1] + inset
  
  if(!missing(pos)){
    if(is.numeric(pos) & length(pos)==2){
      xscale <- pos[1]
      yscale <- pos[2]
    }else{
      if(pos == "bottomleft"){
        xscale <- x[1] + inset * 0.5 
        yscale <- y[1] + inset
      }
    }
  }
  
  
  switch(EXPR = style,
         pretty = {
           segments(xscale, yscale, x1 = xscale + size, yscale, lwd = lwd)
           text(xscale + (size / 2), yscale + inset / 10,
                paste(labelscale,"\n",sep=""), cex = cex)
         }, 
         oldschool = {
           rect(x[2] - size - inset/2, y[1]+inset, 
                x[2]-inset/2, y[1]+(y[2]-y[1])/200+inset/2,
                col = "black", border = "black")
           rect(x[2] - size - inset/2, y[1]+inset, x[2]-inset/2-size/2, 
                y[1]+(y[2]-y[1])/200+inset/2, col = "white", border = "black")
           rect(x[2] - size - inset/2, y[1]+inset, x[2]-inset/2-size+
                  size/4, y[1]+(y[2]-y[1])/200+inset/2, col = "black", 
                border = "black")
           rect(x[2] - size / 4 - inset/2, 
                y[1]+inset, x[2]-inset/2, y[1]+(y[2]-y[1])/200+
                  inset/2, col = "white", border = "black")
           text(x[2] - size / 2 - inset/2,y[1]+(y[2]-y[1])/200+inset,
                paste(labelscale,"\n",sep=""),cex=0.6)
         })
}

#' Convert units
#' @param size a size
#' @param unit_in input unit
#' @param unit_out output unit
#' @noRd
unit_conversion <- function(size, unit_in, unit_out){
  # check args
  if(!unit_in %in% c('km','m','mi')) stop("unit must be 'km', 'm', or 'mi'")
  if(!unit_out %in% c('km','m','mi')) stop("unit must be 'km', 'm', or 'mi'")
  
  if(unit_out == "m"){
    if(unit_in == "km") size <- size * 1000
    if(unit_in == "mi") size <- size * 1609.344
  }
  if(unit_out == "km"){
    if(unit_in == "m") size <- size / 1000
    if(unit_in == "mi") size <- size * 1.609344
  }
  if(unit_out == "mi"){
    if(unit_in == "m") size <- size / 1609.344
    if(unit_in == "km") size <- size / 1.609344
  }
  return(size)
}
