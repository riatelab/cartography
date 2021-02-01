#' @title Legend for Choropleth Maps
#' @description Plot legend for choropleth maps.
#' @name legendChoro
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param breaks break points in sorted order to indicate the intervals for 
#' assigning the colors. 
#' Note that if there are nlevel colors (classes) there should be (nlevel+1) 
#' breakpoints.
#' It is possible to use a vector of characters. 
#' @param col a vector of colors. 
#' @param cex size of the legend. 2 means two times bigger.
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param nodata if TRUE a "no data" box or line is plotted.
#' @param nodata.txt label for "no data" values. 
#' @param nodata.col color of "no data" values.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param symbol type of symbol in the legend 'line' or 'box'
#' @param border color of the box borders
#' @param horiz layout of legend, TRUE for horizontal layout 
#' @export
#' @seealso \link{tc_leg_c}
#' @keywords internal
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' legendChoro(pos = "bottomleft", title.txt = "Title of the legend", title.cex = 0.8,
#'             values.cex = 0.6, breaks = c(1,2,3,4,10.27,15.2),
#'             col = carto.pal(pal1 = "orange.pal",n1 = 5), values.rnd =2,
#'             nodata = TRUE, nodata.txt = "No data available", frame = TRUE, symbol="box")
#' legendChoro(pos = "bottomright", title.txt = "Title of the legend", title.cex = 0.8,
#'             values.cex = 0.6, breaks = c(1,2,5,7,10,15.27),
#'             col = carto.pal(pal1 = "wine.pal",n1 = 5), values.rnd = 0,
#'             nodata = TRUE, nodata.txt = "NA",nodata.col = "black",
#'             frame = TRUE, symbol="line")
#' legendChoro(pos = "topright", title.txt = "Title of the legend", title.cex = 0.8,
#'             values.cex = 0.6,
#'             breaks = c(0,"two","100","1 000","10,000", "1 Million"),
#'             col = carto.pal(pal1 = "orange.pal",n1 = 5), values.rnd =2,
#'             nodata = TRUE, nodata.txt = "No data available", frame = TRUE,
#'             symbol="box")
legendChoro <- function(pos = "topleft", 
                        title.txt = "Title of the legend", 
                        title.cex = 0.8,
                        values.cex = 0.6, 
                        breaks, 
                        col, 
                        cex = 1,
                        values.rnd =2, 
                        nodata = TRUE, 
                        nodata.txt = "No data", 
                        nodata.col = "white",
                        frame=FALSE,symbol="box", 
                        border = "black", horiz = FALSE){
  
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::legendChoro()",
                            with = "tc_leg_c()") 
  
  if (horiz && symbol=="box"){
    legendChoroHoriz(pos = pos, title.txt = title.txt, title.cex = title.cex,
                     values.cex = values.cex, breaks = breaks, col = col, cex = cex,
                     values.rnd = values.rnd, nodata = nodata, nodata.txt = nodata.txt,
                     nodata.col = nodata.col, frame = frame, border = border)
  }else{
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
    width <- (x2 - x1) / (30/cex)
    height <- width / 1.5
    
    # extent
    if(!is.character(breaks)){
      breaks <- as.numeric(round(breaks, values.rnd))
    }
    
    if (nodata == FALSE){nodata.txt <- NULL}
    longval <- max(strwidth(c(breaks, nodata.txt), cex = values.cex))
    legend_xsize <- max(width + longval,
                        strwidth(title.txt, cex = title.cex) - delta2) - delta2
    legend_ysize <- (length(breaks)-1) * height +  strheight(title.txt, cex = title.cex)
    
    # legende_size increase if no.data
    if (nodata == TRUE){legend_ysize <- legend_ysize + height + delta2 }
    
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
    
    # box display
    if (nodata == TRUE){
      rect(xref, yref, xref + width, yref + height,
           col = nodata.col, border = border, lwd = 0.4)
      text(xref + width + delta2 , yref + height / 2, labels = nodata.txt,
           adj = c(0,0.5), cex = values.cex)
      yref <- yref + height + delta2
    }
    
    if (symbol=="box"){
      for (i in 0:(length(breaks)-2)){
        rect(xref, yref + i * height, xref + width, yref + height + i * height,
             col = col[i+1], border = border, lwd = 0.4)
      }
    }else{
      for (i in 0:(length(breaks)-2)){
        segments(xref, yref + height / 2+ i*height, xref + width,
                 yref + i*height + height / 2, lwd = 5, col = col[i+1], lend = 1)
      }
    }
    
    # text display
    for (i in 1:(length(breaks))){
      text(x = xref + width + delta2, y = yref + (i-1) * height,
           labels = breaks[i], adj = c(0,0.5), cex = values.cex)
    }
    
    # title
    text(x = xref, y = yref + (length(breaks)-1) * height + delta1,
         labels = title.txt, adj = c(0,0), cex = title.cex)
  }
}


#' @title  Legend for Typology Maps
#' @description Plot legend for typology maps.
#' @name legendTypo
#' @param categ vector of categories.
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param col a vector of colors. 
#' @param cex size of the legend. 2 means two times bigger.
#' @param nodata if TRUE a "no data" box or line is plotted.
#' @param nodata.txt label for "no data" values. 
#' @param nodata.col color of "no data" values.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param symbol character; 'line' or 'box'
#' @export
#' @seealso \link{tc_leg_t}
#' @keywords internal
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' 
#' # Define labels and colors
#' someLabels <- c("red color", "yellow color", "green color", "black color")
#' someColors <- c("red", "yellow", "green", "black")
#' 
#' # plot legend
#' legendTypo(pos = "bottomleft", title.txt = "Title of the legend", title.cex = 0.8,
#'            values.cex = 0.6, col = someColors, categ = someLabels, 
#'            cex = 0.75,
#'            nodata = TRUE, nodata.txt = "no data", frame = TRUE, symbol="box")
#' legendTypo(pos = "topright", title.txt = "", 
#'            title.cex = 1.5, cex = 1.25,
#'            values.cex = 1, col = someColors, categ = someLabels, 
#'            nodata = FALSE, frame = FALSE, symbol="line")
legendTypo <- function(pos = "topleft", 
                       title.txt = "Title of the legend", 
                       title.cex = 0.8,
                       values.cex = 0.6, 
                       col, categ,
                       cex = 1,
                       nodata = TRUE, 
                       nodata.txt = "No data", 
                       nodata.col = "white",
                       frame=FALSE,
                       symbol="box"){
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::legendTypo()",
                            with = "tc_leg_t()") 
  categ <- rev(as.character(categ))
  col <- rev(col)
  
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
  width <- (x2 - x1) / (30/cex)
  height <- width / 1.5
  
  # xsize
  if (nodata == FALSE){nodata.txt <- NULL}
  longVal <- categ[strwidth(categ, cex = values.cex) == 
                     max(strwidth(categ, cex = values.cex))][1]
  longVal <- max(strwidth(c(longVal, nodata.txt), cex = values.cex))
  legend_xsize <- max(width + longVal, 
                      strwidth(title.txt,cex = title.cex) - delta2) - delta2
  # ysize
  legend_ysize <- (length(categ)) * height + delta2 * (length(categ)) + 
    strheight(title.txt,cex = title.cex) - delta2
  # legende_size increase if no.data
  if (nodata == TRUE){legend_ysize <- legend_ysize + height + delta2 }
  
  
  
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
  
  # box display
  if (nodata == TRUE){
    rect(xref, yref, xref + width, yref + height,
         col = nodata.col, border = "black", lwd = 0.4)
    text(xref + width + delta2 , yref + height / 2, labels = nodata.txt,
         adj = c(0,0.5), cex = values.cex)
    yref <- yref + height + delta2
  }
  
  
  
  if (symbol=="box"){
    for (i in 0:(length(categ)-1)){
      rect(xref, yref + i * height + i * delta2, xref + width, 
           yref + height + i * height + i * delta2, 
           col = col[i + 1], border = "black", lwd = 0.4)
      j <- i+1
      text(x = xref + width + delta2 , 
           y = yref + height / 2 + i * height + i * delta2, 
           labels = categ[j], adj = c(0,0.5), cex = values.cex)
      
    }
  }
  
  if (symbol=="line"){
    for (i in 0:(length(categ)-1)){
      segments(xref, yref + height / 2 + i * height + i * delta2, xref + width, 
               yref + i * height + i * delta2 + height / 2, lwd = 5, 
               col = col[i + 1], lend = 1)
      j <- i+1
      text(xref + width + delta2, 
           y = yref + height / 2 + i * height + i * delta2, labels = categ[j], 
           adj = c(0,0.5), cex = values.cex)
      
    }
  }
  # Affichage du titre
  text(x = xref, 
       y = yref + length(categ) * height + length(categ) * delta2 + delta2, 
       labels = title.txt, adj = c(0,0), cex = title.cex)
  
}


#' @title Legend for Proportional Circles Maps
#' @description Plot legend for proportional circles maps
#' @name legendCirclesSymbols
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var vector of values (at least min and max).
#' @param inches radii of the biggest circle.
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param border color of the borders.
#' @param lwd width of the borders.
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @export
#' @keywords internal
#' @seealso \link{tc_leg_p}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' 
#' propSymbolsLayer(x = mtq, var = "POP",
#'                  inches = 0.2, legend.pos = "n")
#' 
#' legendCirclesSymbols(pos = "topleft", inches = 0.2,
#'                      var = c(min(mtq$POP), max(mtq$POP)))
#' legendCirclesSymbols(pos = "left",
#'                      var = c(min(mtq$POP), max(mtq$POP)),
#'                      inches = 0.2, style = "e")
#' legendCirclesSymbols(pos = "bottomleft",
#'                      var = c(600, 12000, 40000, max(mtq$POP)),
#'                      inches = 0.2, style = "c")
#' legendCirclesSymbols(pos = "topright", cex = 2,
#'                      var = c(600, 30000,max(mtq$POP)),
#'                      inches = 0.2, style = "e", frame = TRUE)
#' legendCirclesSymbols(pos = c(736164.4, 1596658),
#'                      var = c(min(mtq$POP),max(mtq$POP)),
#'                      inches = 0.2, frame = TRUE)
legendCirclesSymbols<- function(pos = "topleft", title.txt = "Title of the legend", 
                                title.cex = 0.8, cex = 1, border="black", lwd=1,
                                values.cex = 0.6, var, inches, col="#E84923", 
                                frame=FALSE, values.rnd=0, style ="c"){
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::legendCirclesSymbols()",
                            with = "tc_leg_p()") 
  var <- abs(var)
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
  
  # Create circles
  ## with unknown intermediates values OR with a list of values
  siz <- sqrt((var * inches * inches * pi  / max(var)) / pi)
  if(length(var) == 2){
    siz <- seq(from = max(siz), to = min(siz), length.out = 4)
    sle <- siz * siz * pi
    var <- sle * max(var) / sle[1]
  }
  size <- xinch(siz)
  var <- round(var,values.rnd)
  size <- sort(size, decreasing = TRUE)
  var <- sort(var, decreasing = TRUE)
  
  # Legend width and height    
  longVal <- var[strwidth(var,cex = values.cex) == 
                   max(strwidth(var, cex = values.cex))][1]
  legend_xsize <- max(size[1] * 2 + strwidth(longVal, cex = values.cex),
                      strwidth(title.txt,cex = title.cex) - delta1)
  if(style == "c"){
    legend_ysize <- size[1] * 2 + strheight(title.txt, cex = title.cex)
  }
  if (style=="e"){
    legend_ysize <- sum(size) * 2 + (length(size) - 1) * delta2 / 2 + 
      strheight(title.txt,cex = title.cex)
  }
  
  # Get legend position
  legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, 
                     y1 = y1, y2 = y2,
                     delta1 = delta1, delta2 = delta2,
                     legend_xsize = legend_xsize, 
                     legend_ysize = legend_ysize)
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  # Frame display
  if(frame == TRUE){
    rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2, border = "black",  col="white")
  }
  
  # legend display
  if(style=="c"){
    for(i in 1:length(size)){
      symbols(x = xref + size[1], y = yref + size[i], circles = size[i],
              add = TRUE, bg = col, inches = FALSE, lwd=lwd, fg=border)
      segments(xref + size[1], yref + size[i] * 2, xref + size[1] * 2 + delta2,
               yref + size[i] * 2, col = border)
      text(x = xref + size[1] * 2 + delta1, y = yref + size[i] * 2, 
           labels = var[i], adj = c(0,0.5), cex = values.cex)
    }
    text(x = xref ,y = yref + delta2 + size[1] * 2 + delta2, title.txt,
         adj = c(0,0), cex = title.cex)
  }
  
  if (style=="e"){
    jump <- 0
    for(i in length(size):1){
      symbols(x = xref + size[1], y = yref + size[i] + jump, circles = size[i],
              add = TRUE, bg = col, inches=FALSE, lwd=lwd, fg=border)
      text(xref + size[1] + size[i] + delta2 , y = yref + size[i] + jump,
           labels = var[i], adj = c(0,0.5), cex = values.cex)
      jump <- jump + size[i] * 2 + delta2 / 2
    }
    text(x = xref, 
         y = yref + sum(size) * 2 + (length(size) - 1) * delta2 / 2 + delta1,
         labels = title.txt, adj = c(0,0), cex = title.cex)
  }
}



#' @title Legend for Proportional Squares Maps
#' @description Plot legend for proportional squares maps
#' @name legendSquaresSymbols
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var vector of values (at least min and max).
#' @param inches length of the sides of the larger square.
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param border color of the borders.
#' @param lwd width of the borders.
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @export
#' @keywords internal
#' @seealso \link{tc_leg_p}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' legendSquaresSymbols(pos = "bottomright", title.txt = "Title of\nthe legend ",
#'                      title.cex = 0.8, values.cex = 0.6,
#'                      var = c(max(mtq$POP), min(mtq$POP)),
#'                      inches = 0.5,
#'                      col="red",  
#'                      frame=TRUE, values.rnd=0, style ="c")
legendSquaresSymbols<- function(pos = "topleft", title.txt = "Title of the legend", 
                                title.cex = 0.8, cex = 1,  border="black", lwd=1,
                                values.cex = 0.6, var, inches, 
                                col="red", frame=FALSE, values.rnd=0, style ="c"){
  
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::legendSquaresSymbols()",
                            with = "tc_leg_p()") 
  var <- abs(var)
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
  
  # Create squares
  ## with unknown intermediates values OR with a list of values
  siz <- sqrt(var * inches * inches / max(var)) 
  if(length(var) == 2){
    siz <- seq(from = max(siz), to = min(siz), length.out = 4)
    sle <- siz * siz
    var <- sle * max(var) / sle[1]
  }
  size <- xinch(siz)
  var <- round(var,values.rnd)
  size <- sort(size, decreasing = TRUE)
  var <- sort(var, decreasing = TRUE)
  
  # xsize & ysize
  # Legend width and height    
  longVal <- var[strwidth(var,cex = values.cex) == 
                   max(strwidth(var, cex = values.cex))][1]
  legend_xsize <- max(size[1] + strwidth(longVal, cex = values.cex),
                      strwidth(title.txt,cex = title.cex) - delta1)
  if(style == "c"){
    legend_ysize <- size[1] + strheight(title.txt, cex = title.cex)
  }
  if (style=="e"){
    legend_ysize <- sum(size) + (length(size) - 1) * delta2 + 
      strheight(title.txt,cex = title.cex)
  }
  
  # Get legend position
  legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                     delta1 = delta1, delta2 = delta2,
                     legend_xsize = legend_xsize, 
                     legend_ysize = legend_ysize)
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  # Frame display
  if(frame == TRUE){
    rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2, border = "black",  col="white")
  }
  
  
  if (style=="c"){
    for(i in 1:length(size)){
      symbols(x = xref + size[i] / 2 + (size[1]-size[i]), y = yref + size[i] / 2, 
              squares = size[i], add = TRUE, bg = col, inches = FALSE, lwd=lwd, 
              fg=border)
      segments(xref + size[1], yref + size[i], xref + size[1] + delta2, 
               yref + size[i], lwd = 0.7, col = border)
      text(x = xref + size[1] + delta1 ,y = yref + size[i], labels = var[i], 
           adj = c(0,0.5), cex = values.cex)
    }
    text(x = xref, y = yref + size[1] + delta1, labels = title.txt, adj = c(0,0),
         cex = title.cex)
  }
  
  if (style=="e"){
    jump <- min(size) / 2
    for(i in length(size):1){
      symbols(x = xref + size[i] / 2 + (size[1] - size[i]) / 2, y = yref + jump, 
              squares = size[i], add = TRUE, bg = col, inches = FALSE, lwd=lwd, 
              fg=border)
      text(xref + size[i] + (size[1] - size[i]) / 2 + delta2,
           y = yref + jump, var[i], adj = c(0, 0.5), cex = values.cex)
      jump <- size[i] / 2 +  size[i - 1] / 2 + jump  + delta2
    }
    text(x = xref ,y = yref + sum(size) + (length(size) - 1) * delta2 + delta1,
         labels = title.txt, adj = c(0,0), cex = title.cex)
  }
}




#' @title Legend for Proportional Bars Maps
#' @description Plot legend for proportional bars maps
#' @name legendBarsSymbols
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var vector of values (at least min and max).
#' @param inches height of the higher bar.
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param border color of the borders.
#' @param lwd width of the borders.
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @export
#' @keywords internal
#' @seealso \link{tc_leg_p}
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' legendBarsSymbols(pos = "topleft", title.txt = "Title of\nthe legend",
#'                      title.cex = 0.8, values.cex = 0.6,cex = 1,
#'                      var = c(min(mtq$POP),max(mtq$POP)),
#'                      inches = 0.5,
#'                      col = "purple",
#'                      values.rnd=0, style ="e")
legendBarsSymbols<- function(pos = "topleft", title.txt = "Title of the legend", 
                             title.cex = 0.8, cex = 1,  border="black", lwd=1,
                             values.cex = 0.6, 
                             var, inches, col = "red", frame = FALSE, 
                             values.rnd = 0, style = "c"){
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::legendBarsSymbols()",
                            with = "tc_leg_p()") 
  var <- abs(var)
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
  
  bwidth <- xinch(inches / 7) 
  
  
  ## with unknown intermediates values OR with a list of values
  siz <- var *  inches / max(var)
  if(length(var) == 2){
    siz <- seq(from = max(siz), to = min(siz), length.out = 4)
    sle <- siz 
    var <- sle * max(var) / sle[1]
  }
  size <- xinch(siz)
  var <- round(var,values.rnd)
  size <- sort(size, decreasing = TRUE)
  var <- sort(var, decreasing = TRUE)
  
  # xsize & ysize
  longVal <- var[strwidth(var,cex = values.cex) == 
                   max(strwidth(var, cex = values.cex))][1]
  legend_xsize <- max(bwidth + strwidth(longVal, cex = values.cex),
                      strwidth(title.txt,cex = title.cex) - delta1)
  if(style == "c"){
    legend_ysize <- size[1] + strheight(title.txt, cex = title.cex) 
  }
  if (style=="e"){
    legend_ysize <- sum(size) + (length(size) - 1) * delta2 + 
      strheight(title.txt,cex = title.cex)
  }
  
  # Get legend position
  legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                     delta1 = delta1, delta2 = delta2,
                     legend_xsize = legend_xsize, 
                     legend_ysize = legend_ysize)
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  # Frame display
  if(frame == TRUE){
    rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2, border = "black",  col="white")
  }
  
  if (style=="c"){
    for(i in 1:length(size)){
      rect(xref, yref, xref + bwidth, yref + size[i], col = col, lwd = lwd, 
           border = border)
      segments(xref + bwidth, yref + size[i], 
               xref + bwidth + delta2, yref + size[i], lwd = 0.7, col = border)
      text(xref + bwidth + delta1, y = yref + size[i], labels = var[i], 
           adj = c(0,0.5), cex = values.cex)
    }
    text(x = xref, y = yref + size[1] + delta1, labels = title.txt, adj = c(0,0),
         cex = title.cex)
  }
  
  if (style=="e"){
    jump <- 0
    for(i in length(size):1){
      rect(xref, yref + jump, xref + bwidth, yref + size[i] + jump, lwd = lwd, 
           border = border, col = col)
      text(xref + bwidth + delta2 ,y = yref + jump + size[i]/2,labels = var[i],
           adj = c(0,0.5), cex = values.cex)
      jump <- size[i] + delta2 + jump
    }
    text(x = xref ,y = yref + sum(size) + (length(size) - 1) * delta2 + delta1,
         labels = title.txt, adj = c(0,0), cex = title.cex)
  }
}



#' @title Legend for Proportional Lines Maps
#' @description Plot legend for proportional lines maps
#' @name legendPropLines
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var vector of values (at least min and max).
#' @param lwd width of the larger line.
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @export
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' legendPropLines(pos = "topleft", title.txt = "Title",
#'                 title.cex = 0.8, values.cex = 0.6, cex = 1,
#'                 var = c(10,100),
#'                 lwd = 15,
#'                 col="red", frame=TRUE, values.rnd=0)
legendPropLines<- function(pos = "topleft", title.txt = "Title of the legend", 
                           title.cex = 0.8, cex = 1,
                           values.cex = 0.6, var, lwd, col="red", frame=FALSE, 
                           values.rnd = 0){
  var <- abs(var)
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
  width <- (x2 - x1) / (30/cex)
  
  ## with unknown intermediates values OR with a list of values
  siz <- var *  lwd / max(var)
  if(length(var) == 2){
    siz <- seq(from = max(siz), to = min(siz), length.out = 4)
    sle <- siz 
    var <- sle * max(var) / sle[1]
  }
  # size <- xinch(siz)
  size <- siz
  var <- round(var,values.rnd)
  size <- sort(size, decreasing = TRUE)
  var <- sort(var, decreasing = TRUE)
  
  # xsize & ysize
  longVal <- var[strwidth(var, cex = values.cex) == max(strwidth(var, cex = values.cex))][1]
  legend_xsize <- max(width + strwidth(longVal, cex = values.cex) - delta2, 
                      strwidth(title.txt, cex = title.cex) - delta1)
  
  legend_ysize <- 8 * delta1 + strheight(title.txt,cex = title.cex)
  
  # Get legend position
  legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                     delta1 = delta1, delta2 = delta2,
                     legend_xsize = legend_xsize, 
                     legend_ysize = legend_ysize)
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  # Frame display
  if(frame == TRUE){
    rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2, border = "black",  col="white")
  }
  
  jump <- delta1
  for(i in 4:1){
    if (size[i] < 0.2){size[i] <- 0.2} # TAILLE DES LIGNE MINIMALES (A METTRE AUSSI SUR LES CARTES)
    segments(xref, yref + jump, xref + width, yref + jump, col = col, 
             lwd = size[i], lend = 1)
    text(xref + width + delta2 ,y= yref + jump, labels = var[i], adj = c(0,0.5),
         cex = values.cex)
    jump <- jump + 2 * delta1 # ICI AMELIORER
  }
  text(x=xref ,y=yref + 9 * delta1, labels = title.txt, adj = c(0,0), 
       cex = title.cex)
}

#' @title Legend for Graduated Size Lines Maps
#' @description Plot legend for graduated size lines maps.
#' @name legendGradLines
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param breaks break points in sorted order to indicate the intervals 
#' for assigning the width of the lines
#' @param lwd  a vector giving the width of the lines.
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @export
#' @seealso \link{tc_leg_gl}
#' @keywords internal
#' @examples 
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' legendGradLines(title.txt = "Title of the legend", 
#'                 pos = "topright",
#'                 title.cex = 0.8,
#'                 values.cex = 0.6, breaks = c(1,2,3,4,10.2,15.2),
#'                 lwd = c(0.2,2,4,5,10),
#'                 col ="blue", values.rnd =2)
legendGradLines <- function(pos = "topleft", title.txt = "Title of the legend", 
                            title.cex = 0.8, cex = 1, values.cex = 0.6, breaks, 
                            lwd, col, values.rnd = 2,frame=FALSE){
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::legendGradLines()",
                            with = "tc_leg_gl()") 
  breaks <- abs(breaks)
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
  width <- (x2 - x1) / (30/cex)
  height <- width /1.5
  
  
  # Taille du bloc de legende
  breaks <- as.numeric(round(breaks, values.rnd))
  longVal <- breaks[strwidth(breaks, cex = values.cex) == 
                      max(strwidth(breaks, cex = values.cex))][1]
  
  legend_xsize <- max(width + strwidth(longVal, cex = values.cex), 
                      strwidth(title.txt,cex = title.cex) - delta2) - delta2
  legend_ysize <- length(breaks) * height + (length(breaks)-2) * delta2 + 
    strheight(title.txt, cex = title.cex)
  
  # Get legend position
  legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                     delta1 = delta1, delta2 = delta2,
                     legend_xsize = legend_xsize, 
                     legend_ysize = legend_ysize)
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  # Frame display
  if(frame == TRUE){
    rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2, border = "black",  col="white")
  }
  
  # Affichage du bloc de legende
  for (i in 0:(length(breaks)-2)){
    j <- i + 1
    segments(xref, 
             yref + height / 2 + i * height + i * delta2 + (height + delta2) / 2, 
             xref + width, 
             yref + i * height + i * delta2 + height / 2 + (height+delta2) / 2, 
             lwd = lwd[j], col = col, lend = 1)
    text(x = xref + width + delta2, 
         y = yref + height / 2 + i * height + i * delta2, labels = breaks[j],
         adj = c(0,0.5), cex = values.cex)
  }
  
  text(x = xref + width + delta2 ,
       y = yref + height/2 + (i+1) * height + (i+1) * delta2, 
       labels = breaks[j+1], adj = c(0,0.5), cex = values.cex)
  
  # Affichage du titre
  text(x = xref, y = yref + length(breaks) * height + length(breaks) * delta2, 
       labels = title.txt, adj = c(0,0), cex = title.cex)
}





#' @title Legend for Double Proportional Triangles Maps
#' @description Plot legends for double proportional triangles maps.
#' @name legendPropTriangles
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var a first vector of positive values.
#' @param var2 a second vector of positive values.
#' @param r a first vector of sizes.
#' @param r2 a second vector of sizes.
#' @param var.txt name of var.
#' @param var2.txt name of var2.
#' @param col color of symbols.
#' @param col2 second color of symbols.
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @export
#' @keywords internal
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' plot(st_geometry(mtq))
#' box()
#' var <- runif(10, 0,100)
#' var2 <- runif(10, 0,100)
#' r <- sqrt(var)*1000
#' r2 <- sqrt(var2)*1000
#' legendPropTriangles(pos = "topright", var.txt = "population 1",
#'                     var2.txt = "population 2", title.txt="Population totale",
#'                     title.cex = 0.8, values.cex = 0.6, cex = 1,
#'                     var = var, var2 = var2, r = r, r2 = r2,
#'                     col="green", col2="yellow", frame=TRUE, values.rnd=2,
#'                     style="c")
legendPropTriangles<- function(pos = "topleft", title.txt, var.txt,var2.txt, 
                               title.cex = 0.8, cex = 1,
                               values.cex = 0.6, var, var2, r, r2, col="red", 
                               col2="blue", frame=FALSE, values.rnd=0, 
                               style="c"){
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::legendPropTriangles()") 
  positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", 
                 "right", "top", "bottom", "middle")
  if(pos %in% positions){
    
    
    # extent
    x1 <- par()$usr[1]
    x2 <- par()$usr[2]
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    xextent <- x2 - x1
    yextent <- y2 - y1
    
    # variables internes
    paramsize1 <- 30/cex
    paramsize2 <- paramsize1*40/25
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    
    
    # TOP
    rValmax1 <- round(max(var,na.rm = TRUE),values.rnd)
    rValmin1 <- round(min(var,na.rm = TRUE),values.rnd)
    rValextent1 <- rValmax1 - rValmin1
    rLegmax1 <- max(r,na.rm = TRUE)
    rLegmin1 <- min(r,na.rm = TRUE)
    rLegextent1 <- rLegmax1 - rLegmin1
    # rVal1 <- c(rValmax1,rValmax1 - rValextent1/3 , rValmax1 - 2*(rValextent1/3),rValmin1)
    rLeg1 <- c(rLegmax1,rLegmax1 - (rLegmax1 - rLegmin1)/2 ,rLegmin1)
    
    
    sleg <- (rLeg1 * rLeg1)/2
    rVal1 <- sleg * rValmax1 / sleg[1]
    rVal1 <- round(rVal1,values.rnd)
    
    # BOTTOM
    rValmax2 <- round(max(var2,na.rm = TRUE),values.rnd)
    rValmin2 <- round(min(var2,na.rm = TRUE),values.rnd)
    rValextent2 <- rValmax2 - rValmin2
    rLegmax2 <- max(r2,na.rm = TRUE)
    rLegmin2 <- min(r2,na.rm = TRUE)
    rLegextent2 <- rLegmax2 - rLegmin2
    # rVal2 <- c(rValmax2,rValmax2 - rValextent2/3 , rValmax2 - 2*(rValextent2/3),rValmin2)
    rLeg2 <- c(rLegmax2,rLegmax2 - (rLegmax2 - rLegmin2)/2 , rLegmin2)
    
    sleg <- (rLeg2 * rLeg2)/2 
    rVal2 <- sleg * rValmax2 / sleg[1]
    rVal2 <- round(rVal2,values.rnd)
    
    
    # xsize & ysize
    xmax <- max(rLegmax2, rLegmax1)
    
    if (style=="e"){
      legend_ysize <- strheight(title.txt,cex=title.cex) + delta2 + rLegmax2/2 + rLegmax1/2 + delta1 + height *2 + delta2 + rLegmin2/2 + rLegmin1/2 + (rLegmin2 + rLegmax2)/4 + + (rLegmin1 + rLegmax1)/4 + 3*delta2
    }
    if (style =="c"){
      legend_ysize <- strheight(title.txt,cex=title.cex) + delta2 + rLegmax2/2 + rLegmax1/2 + delta1 + height *2 + delta2
      
    }
    
    longVal <- rVal1[strwidth(rVal1,cex=values.cex)==max(strwidth(rVal1,cex=values.cex))][1]
    longVal2 <- rVal2[strwidth(rVal2,cex=values.cex)==max(strwidth(rVal2,cex=values.cex))][1]
    if (strwidth(longVal2,cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- longVal2}
    legend_xsize <- strwidth(longVal,cex=values.cex) + max(rLegmax2,rLegmax1)
    tmp <- strwidth(title.txt,cex=title.cex)
    if (tmp > legend_xsize) {legend_xsize <- tmp}
    tmp <- max(strwidth(var.txt,cex=values.cex),strwidth(var2.txt,cex=values.cex)) + height + delta2
    if (tmp > legend_xsize){legend_xsize <- tmp}
    #
    # Position
    if (pos == "bottomleft") {xref <- x1 + delta1 ; yref <- y1 + delta1}
    if (pos == "topleft") {xref <- x1 + delta1 ; yref <- y2 - 2*delta1 - legend_ysize}
    if (pos == "topright") {xref <- x2 - 2*delta1 - legend_xsize ; yref <- y2 -2*delta1 - legend_ysize}
    if (pos == "bottomright") {xref <- x2 - 2*delta1 - legend_xsize ; yref <- y1 + delta1}
    if (pos == "left") {xref <- x1 + delta1 ; yref <- (y1+y2)/2-legend_ysize/2 - delta2}
    if (pos == "right") {xref <- x2 - 2*delta1 - legend_xsize ; yref <- (y1+y2)/2-legend_ysize/2 - delta2}
    if (pos == "top") {xref <- (x1+x2)/2 - legend_xsize/2 ; yref <- y2 - 2*delta1 - legend_ysize}
    if (pos == "bottom") {xref <- (x1+x2)/2 - legend_xsize/2 ; yref <- y1 + delta1}
    if (pos == "middle") { xref <- (x1+x2)/2 - legend_xsize/2 ; yref <- (y1+y2)/2-legend_ysize/2 - delta2}
    
    
    
    # Frame
    if (frame==TRUE){
      rect(xref-delta1, yref-delta1, xref+legend_xsize + delta1*2, yref+legend_ysize + delta1 *2, border = "black",  col="white")
    }
    
    
    symbols(x = xref + max(rLeg1[1],rLeg2[1])/2,y=yref + delta1 - height/2 + height,squares=height/1.5,add=TRUE,bg=col,inches=FALSE)
    symbols(x = xref + max(rLeg1[1],rLeg2[1])/2 ,y=yref + delta1 - height/2,squares=height/1.5,add=TRUE,bg=col2,inches=FALSE)
    text(xref + max(rLeg1[1],rLeg2[1])/2 + height/3 + delta2,yref + delta1/2,var2.txt,adj=c(0,0.5),cex=values.cex)
    text(xref + max(rLeg1[1],rLeg2[1])/2 + height/3 + delta2 , yref + height + delta1/2,var.txt,adj=c(0,0.5),cex=values.cex)
    yref <- yref + height *2 + delta2
    
    
    
    if (style=="e"){
      
      
      text(x=xref ,y=yref + (rLegmax1 + rLegmax2 + rLegmin1 + rLegmin2 + (rLegmax1-rLegmin1)/2 +  (rLegmax2-rLegmin2)/2)/2
           + 4*delta1,adj=c(0,0),title.txt,cex=title.cex)
      
      
      mysize <- rLegmax2
      yadd <-  mysize/2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref + yadd), col = col2)
      segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
      text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,rValmax2,cex=values.cex,adj=c(0,0.5))
      
      mysize <- (rLegmax2 - rLegmin2)/2 ; yadd <-  mysize/2 + yadd + delta2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref+yadd), col = col2)
      segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
      text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,round(rVal2[2],values.rnd),cex=values.cex,adj=c(0,0.5))
      
      mysize <- rLegmin2 ; yadd <-  mysize/2 + yadd + delta2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref+ yadd,yref-mysize/2+yadd,yref+yadd), col = col2)
      segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
      text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,rValmin2,cex=values.cex,adj=c(0,0.5))
      
      
      mysize <- rLegmin1
      yadd <-  yadd + delta2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), 
              c(yref + delta2 + yadd,yref+mysize/2 + delta2 + yadd,yref + delta2 + yadd), 
              col = col)
      segments(xref + xmax/2,
               yref+mysize/2 + delta2 + yadd, 
               xref + xmax/2 + delta1 + xmax/2 ,
               yref+mysize/2 + delta2 + yadd)
      text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,
           yref+mysize/2 + delta2 + yadd,
           rValmin1,
           cex=values.cex,adj=c(0,0.5))
      
      yadd <-  mysize/2 + yadd + delta2
      mysize <- (rLegmax1 - rLegmin1)/2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), 
              c(yref + delta2 + yadd,yref+mysize/2 + delta2  + yadd,yref + delta2 + yadd), 
              col = col )
      segments(xref + xmax/2,yref+mysize/2 + delta2  + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + delta2  + yadd)
      text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,
           yref+mysize/2 + delta2  + yadd,
           round(rVal1[2],values.rnd) ,
           cex=values.cex,adj=c(0,0.5))
      
      yadd <-  mysize/2 + yadd  + delta2 
      mysize <- rLegmax1
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), 
              c(yref + delta2 + yadd,yref+mysize/2 + delta2 + yadd,yref + delta2 + yadd), 
              col = col)
      segments(xref + xmax/2,yref+mysize/2 + delta2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + delta2 + yadd)
      text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + delta2 + yadd,rValmax1,cex=values.cex,adj=c(0,0.5))
      
      
      
    }
    
    if (style=="c"){
      
      text(x=xref ,y=yref + rLegmax2/2 + rLegmax1/2 + delta1 + delta1,adj=c(0,0),title.txt,cex=title.cex)
      
      mysize <- rLegmax2 ;  yadd <-  mysize/2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref + yadd), col = col2)
      segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
      text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,rValmax2,cex=values.cex,adj=c(0,0.5))
      
      mysize <- (rLegmax2 - rLegmin2)/2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref+yadd), col = col2)
      segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
      text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,round(rVal2[2],values.rnd),cex=values.cex,adj=c(0,0.5))
      
      mysize <- rLegmin2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref+ yadd,yref-mysize/2+yadd,yref+yadd), col = col2)
      segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
      text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,rValmin2,cex=values.cex,adj=c(0,0.5))
      
      mysize <- rLegmax1
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + rLegmax2/2 + delta2,yref+mysize/2 + rLegmax2/2 + delta2,yref + rLegmax2/2 + delta2), col = col)
      segments(xref + xmax/2,yref+mysize/2 + rLegmax2/2 + delta2, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2)
      text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2,rValmax1,cex=values.cex,adj=c(0,0.5))
      
      mysize <- (rLegmax1 - rLegmin1)/2
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + rLegmax2/2 + delta2,yref+mysize/2 + rLegmax2/2 + delta2,yref + rLegmax2/2 + delta2), col = col )
      segments(xref + xmax/2,yref+mysize/2 + rLegmax2/2 + delta2, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2)
      text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2,round(rVal1[2],values.rnd) ,cex=values.cex,adj=c(0,0.5))
      
      mysize <- rLegmin1
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + rLegmax2/2 + delta2,yref+mysize/2 + rLegmax2/2 + delta2,yref + rLegmax2/2 + delta2), col = col)
      segments(xref + xmax/2,yref+mysize/2 + rLegmax2/2 + delta2, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2)
      text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2,rValmin1,cex=values.cex,adj=c(0,0.5))
      
    }
  }
}








#' @title Legend for Choropleth Maps
#' @description Plot legend for choropleth maps.
#' @name legendChoroHoriz
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)).
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param breaks break points in sorted order to indicate the intervals for assigning the colors.
#' Note that if there are nlevel colors (classes) there should be (nlevel+1) breakpoints.
#' It is possible to use a vector of characters.
#' @param col a vector of colors.
#' @param cex size of the legend. 2 means two times bigger.
#' @param values.rnd number of decimal places of the values in
#' the legend.
#' @param nodata if TRUE a "no data" box or line is plotted.
#' @param nodata.txt label for "no data" values.
#' @param nodata.col color of "no data" values.
#' @param frame whether to add a frame to the legend (TRUE) or
#' not (FALSE).
#' @param border color of the box borders
#' @noRd
legendChoroHoriz <- function(pos = "topleft",
                             title.txt = "Title of the legend",
                             title.cex = 0.8,
                             values.cex = 0.6,
                             breaks,
                             col,
                             cex = 1,
                             values.rnd =2,
                             nodata = TRUE,
                             nodata.txt = "No data",
                             nodata.col = "white",
                             frame=FALSE,
                             border = NA){
  
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
  width <- (x2 - x1) / (20/cex)
  height <- (x2 - x1) / (35/cex) / 1.5
  
  # extent
  if(!is.character(breaks)){
    breaks <- as.numeric(round(breaks, values.rnd))
  }
  
  if (nodata == FALSE){nodata.txt <- NULL}
  
  longval1 <- strwidth(breaks[1], cex = values.cex)
  longval2 <- strwidth(breaks[length(breaks)], cex = values.cex)
  
  
  legend_xsize <- ((length(breaks)-1) * width) + (longval1 + longval2) / 2 
  legend_ysize <- height + delta2+ strheight(title.txt, cex = title.cex)
  
  # legende_size increase if no.data
  if (nodata == TRUE){
    legend_xsize <- legend_xsize + width + delta2
    lnodata <- strwidth(nodata.txt, cex = values.cex)
    nddiff <- lnodata - width
    if(nddiff>0){
      legend_xsize  <- legend_xsize + nddiff 
    }
  }
  
  ltitle <- strwidth(title.txt, cex = title.cex) + longval1/2
  legend_xsize <- max(ltitle, legend_xsize)
  
  # Get legend position
  legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
                     delta1 = delta1, delta2 = delta2,
                     legend_xsize = legend_xsize,
                     legend_ysize = legend_ysize)
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  if(length(pos)==1){
    if(substr(pos, nchar(pos)-1,nchar(pos))=="ht"){
      xref <- xref + delta1
    }
  }
  
  # Frame
  if (frame==TRUE){
    rect(xref - delta1, 
         yref - delta1, 
         xref + legend_xsize + delta1,
         yref + legend_ysize + 2*delta1,  col="white")
  }
  
  # no data box display
  if (nodata == TRUE){
    rect(xref + (length(breaks)-1) * width + delta2 + (longval1 + longval2)/2 + nddiff/2, 
         yref + delta2, 
         xref + ((length(breaks)-1) * width) + width + delta2 + (longval1 + longval2)/2 + nddiff/2, 
         yref + height + delta2,
         col = nodata.col, border = border, lwd = 0.4)
    text(x =  xref + ((length(breaks)-1) * width) +  delta2 + (longval1 + longval2)/2 + width/2 + nddiff/2,
         y = yref  , labels = nodata.txt,
         adj = c(0.5,0.5), cex = values.cex)
  }
  
  # boxes
  for (i in 0:(length(breaks)-2)){
    rect(xref + i * width + longval1 / 2, 
         yref + delta2, 
         xref + width + i * width + longval1 / 2, 
         yref + height + delta2,
         col = col[i+1], border =border, lwd = 0.4)
  }
  
  # text display
  for (i in 1:(length(breaks))){
    text(x = xref + (i-1) * width + longval1/2, 
         y = yref,
         labels = breaks[i], adj = c(0.5,0.5), cex = values.cex)
  }
  
  # title
  text(x = xref + longval1/2, y = yref + height + delta1 + delta2,
       labels = title.txt, adj = c(0,0), cex = title.cex)
}
