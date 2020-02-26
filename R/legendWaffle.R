#' @title  Legend for Typology Maps
#' @description Plot legend for typology maps.
#' @name legendWaffle
#' @param categ vector of categories.
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param col a vector of colors. 
#' @param cell.size size of the cell
#' @param cex size of the legend. 2 means two times bigger.
#' @param cell.txt label for cell values. 
#' @param border color of the cells borders.
#' @param lwd width of the cells borders
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @export
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' 
#' # Define labels and colors
#' someLabels <- c("red color", "yellow color", "green color", "black color")
#' someColors <- c("red", "yellow", "green", "black")
legendWaffle <- function(pos = "topleft", 
                         title.txt = "Title of the legend", 
                         title.cex = 0.8,
                         values.cex = 0.6, 
                         categ,
                         cex = 1,
                         cell.txt = "1 cell = ...", 
                         col,
                         cell.size, 
                         border = "white", 
                         lwd = .2,
                         frame = FALSE){
  
  categ <- (as.character(categ))
  col <- (col)
  
  # exit for none
  positions <- c("bottomleft", "topleft", "topright", "bottomright",
                 "left", "right", "top", "bottom", "center", 
                 "bottomleftextra")
  if(length(pos) == 1){if(!pos %in% positions){return(invisible())}}
  
  # figdim in geo coordinates
  x1 <- par()$usr[1]
  x2 <- par()$usr[2]
  y1 <- par()$usr[3]
  y2 <- par()$usr[4]
  
  # offsets
  delta1 <- xinch(0.15) * cex
  delta2 <- delta1 / 2
  
  
  # variables internes
  width <- (x2 - x1) / (50/cex)
  if(!missing(cell.size)){
    width <- cell.size
  }
  height <- width 
  
  # xsize
  longVal <- categ[strwidth(categ, cex = values.cex) == 
                     max(strwidth(categ, cex = values.cex))][1]
  longVal <- max(c(strwidth(longVal, cex = values.cex), 
                   (strwidth(cell.txt, cex = values.cex) - width - delta2)))
  legend_xsize <- max(width + longVal, 
                      strwidth(title.txt,cex = title.cex) - delta2) - delta2
  # ysize
  legend_ysize <- (length(categ)) * height + delta2 * (length(categ)) + 
    strheight(title.txt,cex = title.cex) - delta2
  legend_ysize <- legend_ysize + height + delta2
  
  # Get legend position
  legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                     delta1 = delta1, delta2 = delta2,
                     legend_xsize = legend_xsize,
                     legend_ysize = legend_ysize)
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  # Frame
  if (frame==TRUE){
    rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2, border = "black",  col="white")
  }
  
  text(xref , yref + height / 2, labels = cell.txt,
       adj = c(0,0.5), cex = values.cex)
  yref <- yref + height + delta2
  
  
  for (i in 0:(length(categ)-1)){
    rect(xref,
         yref + i * height + i * delta2, 
         xref + width, 
         yref + height + i * height + i * delta2, 
         col = col[i + 1], 
         border = border, 
         lwd = lwd
    )
    
    j <- i+1
    text(x = xref + width + delta2 , 
         y = yref + height / 2 + i * height + i * delta2, 
         labels = categ[j], adj = c(0,0.5), cex = values.cex)
    
  }
  
  
  
  # Affichage du titre
  text(x = xref, 
       y = yref + length(categ) * height + length(categ) * delta2 + delta2, 
       labels = title.txt, adj = c(0,0), cex = title.cex)
  
}