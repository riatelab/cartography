#' @title Layout Layer
#' @description Plot a layout layer.
#' @name layoutLayer
#' @param title title of the map.
#' @param sources sources of the map (or something else).
#' @param author author of the map (or something else).
#' @param scale size of the scale bar in kilometers. If set to FALSE, no scale 
#' bar is displayed, if set to "auto" an automatic size is used (1/10 of the map 
#' width).
#' @param posscale position of the scale, can be "bottomright", "bottomleft" or 
#' a vector of two coordinates (c(x, y))
#' @param frame whether displaying a frame (TRUE) or not (FALSE).
#' @param col color of the title box and frame border.
#' @param coltitle color of the title.
#' @param tabtitle size of the title box either a full banner (FALSE) or a 
#' "tab" (TRUE). 
#' @param postitle position of the title, one of "left", "center", "right". 
#' @param bg color of the frame background.
#' @param north whether displaying a North arrow (TRUE) or not (FALSE).
#' @param south whether displaying a South arrow (TRUE) or not (FALSE).
#' @param extent sf object or Spatial*DataFrame; sets the extent of the frame to 
#' the one of a spatial object. (optional)
#' @param theme name of a cartographic palette (see \link{carto.pal.info}). 
#' col and coltitle are set according to the chosen palette. 
#' @param horiz orientation of sources and author. TRUE for horizontal display 
#' on the bottom left corner, FALSE for vertical display on the bottom right 
#' corner.
#' @details If extent is not set, plot.new has to be called first.\cr
#' The size of the title box in layoutLayer is fixed to 1.2 lines height.
#' @export 
#' @keywords internal
#' @seealso \link{tc_layout}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq), col = "#D1914D", border = "white", bg = "#A6CAE0")
#' # Layout plot
#' layoutLayer()
#' 
#' plot(st_geometry(mtq), col = "#D1914D", border = "white", bg = "#A6CAE0")
#' # Layout plot
#' layoutLayer(title = "Martinique",
#'             author =  paste0("cartography ", packageVersion("cartography")),
#'             tabtitle = TRUE, scale = 5, north = TRUE, frame = FALSE,
#'             theme = "sand.pal")
layoutLayer <- function(title = "Title of the map, year",
                        sources = "", author = "",
                        horiz = TRUE, 
                        col = "black", coltitle = "white", theme = NULL, 
                        bg = NULL, scale = "auto", posscale = "bottomright",
                        frame = TRUE, north = FALSE, 
                        south = FALSE, extent = NULL, tabtitle = FALSE, 
                        postitle = "left"){
  lifecycle::deprecate_soft(
    when = "3.0.0", 
    what = "cartography::layoutLayer()",
    with = "tc_layout()", 
    details = paste0("You can also use `tc_title()`, `tc_credits()`, ", 
                     "`tc_arrow()` or `tc_scale()` to set layout elements ", 
                     "individually.")
    ) 
  # Extent
  if (!is.null(extent)){
    if(methods::is(extent, 'Spatial')){
      extent <- sf::st_as_sf(extent)
    }
    plot(sf::st_geometry(extent), border = NA, col = NA, add = FALSE, bg = bg)
    mapExtent <- par()$usr
  } else {
    mapExtent <- par()$usr
  }
  x1 <- mapExtent[1]
  x2 <- mapExtent[2]
  y1 <- mapExtent[3]
  y2 <- mapExtent[4]
  delta <- min((y2 - y1) / 40, (x2 - x1) / 40)
  
  # Color theme
  if(!is.null(theme)){
    pals <- carto.pal.info()[1:14]
    if (theme %in% pals){
      pal <- carto.pal(theme, 20)
      col <- pal[7]
      coltitle <- pal[19]
    }
  }
  
  # Frame
  if(frame == TRUE){
    rect(x1, y1, x2, y2, border = col, col = bg)
  }
  
  # Scale
  if (!is.null(scale)){
    if(scale == 0){
      # DO NOTHING
    }else{
      if(scale=="auto"){
        barscale(style = "pretty", pos = posscale)
      }else{
        barscale(size = scale, style = "pretty", pos=posscale)
      }
    }
  }
  
  # North arrow
  if(south){
    north <- FALSE
    north(pos = "topright", south = TRUE)
  }
  if(north){
    north(pos = "topright")
  }
  
  # Title
  if(title != ""){
    size <- 0.8
    titlesize <- xinch(strwidth(title, cex = size, units = "inches", font = 2))
    par(xpd = TRUE)
    switch(postitle, 
           left = {
             x1s <- x1
           }, 
           center = {
             x1s <- x1 + ((x2 - x1) - titlesize) / 2 - delta / 2
           }, 
           right = {
             x1s <- x1 + ((x2 - x1) - titlesize) - delta
           }
    )
    
    if(tabtitle){
      rect(xleft = x1s, 
           ybottom = y2, 
           xright = x1s + titlesize + delta, 
           ytop = y2 + (xinch(1.2) * 0.2), 
           border = col, col = col)
    }else{
      rect(xleft = x1, 
           ybottom = y2, 
           xright = x2, 
           ytop = y2 + (xinch(1.2) * 0.2), 
           border = col, col = col)
    }
    
    text(x = x1s + delta / 2, 
         y = y2 + ((xinch(1.2) * 0.2) - xinch(strheight(title, cex = 0.8, 
                                                        units = "inches", 
                                                        font = 2))) / 2,
         labels = title, adj = c(0,0),
         cex = size, col = coltitle, font = 2)
    par(xpd = FALSE)
  }
  
  # Credits
  if(author != ""){sep = "\n"}else{sep = ""}
  credit <- paste(sources, author, sep = sep)
  if(horiz){
    text(x = x1 + delta / 2, y = y1 + delta / 2, labels = credit,
         adj = c(0,0), cex = 0.6, font = 3)
  }else{
    text(x = x2 - delta / 2, y = y1 + delta / 2, labels = credit,
         adj = c(0,0), cex = 0.6, font = 3, srt = 90)
  }
  
}
