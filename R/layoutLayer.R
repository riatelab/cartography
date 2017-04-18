#' @title Layout Layer
#' @description Plot a layout layer.
#' @name layoutLayer
#' @param title title of the map.
#' @param sources sources of the map (or something else).
#' @param author author of the map (or something else).
#' @param scale size of the scale bar in kilometers. If set to NULL, no scale bar is 
#' displayed, if set to 0 an automatic scale bar is displayed (1/10 of the map width).
#' @param frame wheither displaying a frame (TRUE) or not (FALSE).
#' @param col color of the title box and frame border.
#' @param coltitle color of the title.
#' @param bg color of the frame background.
#' @param north wheither displaying a Noth arrow (TRUE) or not (FALSE).
#' @param south wheither displaying a South arrow (TRUE) or not (FALSE).
#' @param extent sf object or Spatial*DataFrame; sets the extent of the frame to 
#' the one of a spatial object. (optional)
#' @param theme name of a cartographic palette (see \link{carto.pal.info}). 
#' col and coltitle are set according to the chosen palette. 
#' @details If extent is not set, plot.new has to be called first.\cr
#' The size of the title box in layoutLayer is fixed to 1.2 lines height.
#' @export
#' @seealso \link{labelLayer}
#' @examples
#' data("nuts2006")
#' # Example 1
#' plot(nuts0.spdf, col = "grey60",border = "grey20", add=FALSE)
#' # Layout plot
#' layoutLayer()
#' 
#' # Example 2
#' plot(nuts0.spdf, col=NA, border = NA, bg ="#A6CAE0")
#' plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
#' plot(nuts0.spdf, col = "#D1914D",border = "white", lwd=1, add=TRUE)
#' layoutLayer(col = NA, coltitle = "black",
#'             sources = "", author = "",
#'             frame = FALSE,
#'             south = TRUE)
#'             
#' # Example 3
#' nuts3.df$gdphab <- 1000000 * nuts3.df$gdppps2008 / nuts3.df$pop2008
#' choroLayer(spdf = nuts3.spdf, df = nuts3.df, var = "gdphab",
#'            legend.pos = "right", border = NA, nclass = 6,
#'            col = carto.pal('green.pal', 6))
#' # Layout plot
#' layoutLayer(title = "GDP per Inhabitants", sources = "",
#'             author = "Eurostat, 2008", theme = "green.pal")
layoutLayer <- function(title = "Title of the map, year",
                        sources = "Source(s)", author = "Author(s)",
                        col = "black", coltitle = "white", theme = NULL, 
                        bg = NULL, scale = 0, frame = TRUE, north = TRUE, 
                        south = FALSE, extent = NULL){
  # EXTENT
  if (!is.null(extent)){
    if(methods::is(extent, 'Spatial')){
      extent <- sf::st_as_sf(extent)
      }
    plot(sf::st_geometry(extent), border = NA, col = NA, add = FALSE)
    mapExtent <- par()$usr
  } else {
    mapExtent <- par()$usr
  }
  x1 <- mapExtent[1]
  x2 <- mapExtent[2]
  y1 <- mapExtent[3]
  y2 <- mapExtent[4]
  delta <- min((y2 - y1) / 40, (x2 - x1) / 40)
  
  # COLOR THEME
  if(!is.null(theme)){
    pals <- carto.pal.info()[1:14]
    if (theme %in% pals){
      pal <- carto.pal(theme, 20)
      col <- pal[7]
      coltitle <- pal[19]
    }
  }
  
  # FRAME
  if(frame == TRUE){
    rect(x1, y1, x2, y2, border = col, col = bg)
  }

  # SCALE
  if (!is.null(scale)){
    if(scale == 0){
      scale <- NULL
    }
    barscale(size = scale, style = "pretty")
  }
  
  # NORTH
  if(south == T){
    north <- FALSE
    north(pos = "topright", south = TRUE)
  }
  if(north == T){
    north(pos = "topright")
  }

  
  # TITLE
  size <- 0.8
  par(xpd = TRUE)
  rect(xleft = x1, ybottom = y2, xright = x2, ytop = y2 + (xinch(1.2) * 0.2), 
       border = col, col = col)
  text(x = x1 + delta / 2, 
       y = y2 + ((xinch(1.2) * 0.2) - 
                   xinch(strheight(title, cex = 0.8, units = "inches"))) / 2,
       labels = title, adj = c(0,0),
       cex = size, col = coltitle, font = 2)
  par(xpd = FALSE)

  # SOURCES
  text(x1 + delta / 2, y1 + delta / 2, paste(sources, author, sep = "\n"),
       adj = c(0,0), cex = 0.6, font = 3)
}
