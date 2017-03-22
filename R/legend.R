# #' @title Legend for Choropleth Maps
# #' @description Plot legend for choropleth maps.
# #' @name legendChoro
# #' @param pos position of the legend, one of "topleft", "top", 
# #' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
# #' @param title.txt title of the legend.
# #' @param title.cex size of the legend title.
# #' @param values.cex size of the values in the legend.
# #' @param breaks break points in sorted order to indicate the intervals for assigning the colors. 
# #' Note that if there are nlevel colors (classes) there should be (nlevel+1) breakpoints.
# #' @param col a vector of colors. 
# #' @param cex size of the legend. 2 means two times bigger.
# #' @param values.rnd number of decimal places of the values in 
# #' the legend.
# #' @param nodata if TRUE a "no data" box or line is plotted.
# #' @param nodata.txt label for "no data" values. 
# #' @param nodata.col color of "no data" values.
# #' @param frame whether to add a frame to the legend (TRUE) or 
# #' not (FALSE).
# #' @param symbol type of symbol in the legend 'line' or 'box'
# #' @export
# #' @examples
# #' data("nuts2006")
# #' plot(nuts0.spdf, col = "grey")
# #' box()
# #' legendChoro(pos = "bottomleft", title.txt = "Title of the legend", title.cex = 0.8,
# #'             values.cex = 0.6, breaks = c(1,2,3,4,10.27,15.2),
# #'             col = carto.pal(pal1 = "orange.pal",n1 = 5), values.rnd =2,
# #'             nodata = TRUE, nodata.txt = "No data available", frame = TRUE, symbol="box")
# #' legendChoro(pos = "bottomright", title.txt = "Title of the legend", title.cex = 0.8,
# #'             values.cex = 0.6, breaks = c(1,2,5,7,10,15.27),
# #'             col = carto.pal(pal1 = "wine.pal",n1 = 5), values.rnd = 0,
# #'             nodata = TRUE, nodata.txt = "NA",nodata.col = "black",
# #'             frame = TRUE, symbol="line")
# legendChoro <- function(pos = "topleft", 
#                         title.txt = "Title of the legend", 
#                         title.cex = 0.8,
#                         values.cex = 0.6, 
#                         breaks, 
#                         col, 
#                         cex = 1,
#                         values.rnd =2, 
#                         nodata = TRUE, 
#                         nodata.txt = "No data", 
#                         nodata.col = "white",
#                         frame=FALSE,symbol="box"){
#   
#   positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", 
#                  "right", "top", "bottom", "middle")
#   cex = 1
#   if(pos %in% positions){
#     # extent
#     legref <- function(cex){
#       x1 <- par()$usr[1]
#       x2 <- par()$usr[2]
#       y1 <- par()$usr[3]
#       y2 <- par()$usr[4]
#       xextent <- x2 - x1
#       yextent <- y2 - y1
#       paramsize1 = 30 / cex
#       paramsize2 <- paramsize1 * 40 / 25
#       width <- xextent / paramsize1
#       height <- width /1.5
#       delta1 <- min(yextent / paramsize2, xextent / paramsize2) # Gros eccart entre les objets
#       delta2 <- delta1 / 2 # Petit eccart entre les objets
#       return(list(x1 = x1, x2 = x2, y1 = y1, y2 = y2, xextent = xextent, 
#                   yextent = yextent, 
#                   width = width, height = height, delta1 = delta1, 
#                   delta2 = delta2))
#     }
#     lp <- legref(cex = cex)
#     names(lp)
#     
#     breaks = c(0.2,10,100,125,10)
#     values.rnd = 2
#     nodata = FALSE
#     values.cex = 0.6
#     title.cex = 0.8
#     title.txt = "Check my extra extra long title"
#     nodata.col = "red"
#     col = c("red", "yellow", "blue", "green")
#     
#     # Taille du bloc de legende
#     breaks <- as.numeric(round(breaks, values.rnd))
#     if (nodata == FALSE){nodata.txt <- NULL}
#     longval <- max(strwidth(c(breaks, nodata.txt), cex = values.cex))
#     
#     
#     # legend_xsize <- max(lp$width + strwidth(longval, cex = values.cex), 
#     #                     strwidth(title.txt, cex = title.cex) - lp$delta2) - lp$delta2
#     legend_xsize <- max(lp$width + strwidth(longval, cex = values.cex) + lp$delta2, 
#                         strwidth(title.txt, cex = title.cex))  
#     
#     legend_ysize <- (length(breaks)-1) * lp$height +  strheight(title.txt, cex = title.cex)
#     
#     # legende_size augmente si un caisson no data
#     if (nodata == TRUE){legend_ysize <- legend_ysize + lp$height + lp$delta2 }
#     
#     # Position
#     if (pos == "bottomleft") {
#       xref <- lp$x1 + lp$delta1 
#       yref <- lp$y1 + lp$delta1
#     }
#     if (pos == "topleft") {
#       xref <- lp$x1 + lp$delta1
#       yref <- lp$y2 - 2 * lp$delta1 - legend_ysize
#     }
#     if (pos == "topright") {
#       xref <- x2 - 2*delta1 - legend_xsize
#       yref <- y2 -2*delta1 - legend_ysize
#     }
#     if (pos == "bottomright") {
#       xref <- x2 - 2*delta1 - legend_xsize 
#       yref <- y1 + delta1}
#     if (pos == "left") {
#       xref <- x1 + delta1 
#       yref <- (y1+y2)/2-legend_ysize/2 - delta2
#     }
#     if (pos == "right") {
#       xref <- x2 - 2*delta1 - legend_xsize
#       yref <- (y1+y2)/2-legend_ysize/2 - delta2
#     }
#     if (pos == "top") {
#       xref <- (x1+x2)/2 - legend_xsize/2 
#       yref <- y2 - 2*delta1 - legend_ysize
#     }
#     if (pos == "bottom") {
#       xref <- (x1+x2)/2 - legend_xsize/2 
#       yref <- y1 + delta1
#     }
#     if (pos == "middle") {
#       xref <- (x1+x2)/2 - legend_xsize/2 
#       yref <- (y1+y2)/2-legend_ysize/2 - delta2
#     }
#     
#     plot(1:10)
#     points(xref, yref)
#     
#     
#     # Frame
#     if (frame==TRUE){
#       rect(xref - lp$delta1, 
#            yref - lp$delta1, 
#            xref + legend_xsize + lp$delta1 * 2, 
#            yref + legend_ysize + lp$delta1 * 2, 
#            border = "black",  col="white")
#     }
#     
#     # Affichage du bloc de legende
#     
#     if (nodata == TRUE){
#       rect(xref,
#            yref,
#            xref + lp$width,
#            yref + lp$height,
#            col = nodata.col, border = "black", lwd = 0.4)
#       text(xref + width + delta2 ,yref + height/2 ,nodata.txt,adj=c(0,0.5),cex=values.cex)
#       yref <- yref + height + delta2
#     }
#     
#     if (symbol=="line"){
#       for (i in 0:(length(breaks)-2)){
#         segments(xref, yref + height/2+ i*height, xref + width, yref + i*height + height/2, lwd=5, col=col[i+1], lend=1)
#       }
#     } else { #box
#       for (i in 0:(length(breaks)-2)){
#         rect(xref,
#              yref + i * lp$height,
#              xref + lp$width,
#              yref + lp$height + i * lp$height,
#              col = col[i+1], border = "black", lwd = 0.4)
#       }
#       
#     }
#     
#     # Affichage des textes
#     for (i in 1:(length(breaks))){
#       text(x = xref + lp$width + lp$delta2 ,
#            y = yref + (i-1) * lp$height,
#            labels = breaks[i],
#            adj = c(0,0.5), cex = values.cex)
#     }
#     
#     # Affichage du titre
#     text(x = xref, y = yref + (length(breaks)-1) * lp$height + lp$delta1, 
#          labels = title.txt, adj = c(0,0), cex = title.cex)
#     
#   }
#   
# }