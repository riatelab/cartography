# legpos <- function(pos, x1, x2, y1, y2, delta1, delta2, legend_xsize, legend_ysize){
#   # Position
#   if(length(pos) == 2){
#     return(list(xref = pos[1], yref = pos[2]))
#   }
#   if (pos == "bottomleft") {
#     xref <- x1 + delta1
#     yref <- y1 + delta1
#   }
#   if (pos == "topleft") {
#     xref <- x1 + delta1
#     yref <- y2 - 2 * delta1 - legend_ysize
#   }
#   if (pos == "topright") {
#     xref <- x2 - 2 * delta1 - legend_xsize
#     yref <- y2 -2 * delta1 - legend_ysize
#   }
#   if (pos == "bottomright") {
#     xref <- x2 - 2 * delta1 - legend_xsize
#     yref <- y1 + delta1
#   }
#   if (pos == "left") {
#     xref <- x1 + delta1
#     yref <- (y1+y2)/2-legend_ysize/2 - delta2
#   }
#   if (pos == "right") {
#     xref <- x2 - 2*delta1 - legend_xsize
#     yref <- (y1+y2)/2-legend_ysize/2 - delta2
#   }
#   if (pos == "top") {
#     xref <- (x1+x2)/2 - legend_xsize/2
#     yref <- y2 - 2*delta1 - legend_ysize
#   }
#   if (pos == "bottom") {
#     xref <- (x1+x2)/2 - legend_xsize/2
#     yref <- y1 + delta1
#   }
#   if (pos == "center") {
#     xref <- (x1+x2)/2 - legend_xsize/2
#     yref <- (y1+y2)/2-legend_ysize/2 - delta2
#   }
#   return(list(xref = xref, yref = yref))
# }
# 
# 
# legendChoro2 <- function(pos = "topleft",
#                          title.txt = "Title of the legend",
#                          title.cex = 0.8,
#                          values.cex = 0.6,
#                          breaks,
#                          col,
#                          cex = 1,
#                          values.rnd =2,
#                          nodata = TRUE,
#                          nodata.txt = "No data",
#                          nodata.col = "white",
#                          frame=FALSE,symbol="box"){
# 
#   # exit for none
#   positions <- c("bottomleft", "topleft", "topright", "bottomright",
#                  "left", "right", "top", "bottom", "center")
#   if(length(pos) == 1){if(!pos %in% positions){return()}}
# 
#   # figdim in geo coordinates
#   x1 <- par()$usr[1]
#   x2 <- par()$usr[2]
#   y1 <- par()$usr[3]
#   y2 <- par()$usr[4]
#   
#   # offsets
#   delta1 <- xinch(0.15) * cex
#   delta2 <- delta1 / 2
# 
#   # variables internes
#   width <- (x2 - x1) / (30/cex)
#   height <- width / 1.5
# 
#   # extent
#   breaks <- as.numeric(round(breaks, values.rnd))
#   
#   if (nodata == FALSE){nodata.txt <- NULL}
#   longval <- max(strwidth(c(breaks, nodata.txt), cex = values.cex))
#   legend_xsize <- max(width + longval,
#                       strwidth(title.txt, cex = title.cex) - delta2) - delta2
#   legend_ysize <- (length(breaks)-1) * height +  strheight(title.txt, cex = title.cex)
#   
#   # legende_size increase if no.data
#   if (nodata == TRUE){legend_ysize <- legend_ysize + height + delta2 }
#   
#   # Get legend position
#   legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2,
#                      delta1 = delta1, delta2 = delta2,
#                      legend_xsize = legend_xsize, 
#                      legend_ysize = legend_ysize)
#   xref <- legcoord$xref
#   yref <- legcoord$yref
#   
#   # Frame
#   if (frame==TRUE){
#     rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2,
#          yref + legend_ysize + delta1 * 2, border = "black",  col="white")
#   }
#   
#   # box display
#   if (nodata == TRUE){
#     rect(xref, yref, xref + width, yref + height,
#          col = nodata.col, border = "black", lwd = 0.4)
#     text(xref + width + delta2 , yref + height / 2, labels = nodata.txt,
#          adj = c(0,0.5), cex = values.cex)
#     yref <- yref + height + delta2
#   }
#   
#   if (symbol=="box"){
#     for (i in 0:(length(breaks)-2)){
#       rect(xref, yref + i * height, xref + width, yref + height + i * height,
#            col = col[i+1], border = "black", lwd = 0.4)
#     }
#   }else{
#     for (i in 0:(length(breaks)-2)){
#       segments(xref, yref + height / 2+ i*height, xref + width, 
#                yref + i*height + height / 2, lwd = 5, col = col[i+1], lend = 1)
#     }
#   }
#   
#   # text display
#   for (i in 1:(length(breaks))){
#     text(x = xref + width + delta2, y = yref + (i-1) * height, 
#          labels = breaks[i], adj = c(0,0.5), cex = values.cex)
#   }
#   
#   # title
#   text(x = xref, y = yref + (length(breaks)-1) * height + delta1,
#        labels = title.txt, adj = c(0,0), cex = title.cex)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# legendBarsSymbols<- function(pos = "topleft", title.txt = "Title of the legend", 
#                              title.cex = 0.8, cex = 1,
#                              values.cex = 0.6, var, r, breakval = NULL, 
#                              col="red", col2="blue", frame=FALSE, values.rnd=0, style ="c"){
#   
#   var <- abs(var)
#   positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "middle")
#   if(pos %in% positions){
#     
#     # extent
#     x1 <- par()$usr[1]
#     x2 <- par()$usr[2]
#     y1 <- par()$usr[3]
#     y2 <- par()$usr[4]
#     xextent <- x2 - x1
#     yextent <- y2 - y1
#     
#     # variables internes
#     paramsize1 = 30/cex
#     paramsize2 <- paramsize1*40/25
#     width <- (x2 - x1) / 40
#     height <- width /1.5
#     delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
#     delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
#     
#     
#     rValmax <- max(var,na.rm = TRUE)
#     rValmin <- min(var,na.rm = TRUE)
#     rValextent <- rValmax - rValmin
#     rLegmax <- max(r,na.rm = TRUE)
#     rLegmin <- min(r,na.rm = TRUE)
#     rLegextent <- rLegmax - rLegmin
#     
#     rLeg <- c(rLegmax,rLegmax - rLegextent/3 , rLegmax - 2*(rLegextent/3),rLegmin)
#     
#     sleg <- rLeg 
#     rVal <- sleg * rValmax / sleg[1]
#     rVal <- round(rVal,values.rnd)
#     
#     
#     
#     
#     
#     # xsize & ysize
#     
#     if (style=="c"){
#       longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
#       if(!is.null(breakval)){if (strwidth(paste (">=",format(breakval,scientific=FALSE)),cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- paste (">=",format(breakval,scientific=FALSE))}}
#       legend_xsize <- max(height/1.5 + strwidth(longVal,cex=values.cex),strwidth(title.txt,cex = title.cex)-delta1)
#       
#       legend_ysize <-rLeg[1] + strheight(title.txt,cex = title.cex) - delta1
#       if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2}
#     }
#     
#     if (style=="e"){
#       longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
#       if(!is.null(breakval)){if (strwidth(paste (">=",format(breakval,scientific=FALSE)),cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <-paste (">=",format(breakval,scientific=FALSE))}}
#       legend_xsize <- max(height/1.5 + strwidth(longVal,cex=values.cex)-delta2,strwidth(title.txt,cex = title.cex)-delta1)
#       
#       legend_ysize <-rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + strheight(title.txt,cex = title.cex)- delta1
#       if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2}
#     }
#     
#     # Position
#     if (pos == "bottomleft") {xref <- x1 + delta1 ; yref <- y1 + delta1}
#     if (pos == "topleft") {xref <- x1 + delta1 ; yref <- y2 - 2*delta1 - legend_ysize}
#     if (pos == "topright") {xref <- x2 - 2*delta1 - legend_xsize ; yref <- y2 -2*delta1 - legend_ysize}
#     if (pos == "bottomright") {xref <- x2 - 2*delta1 - legend_xsize ; yref <- y1 + delta1}
#     if (pos == "left") {xref <- x1 + delta1 ; yref <- (y1+y2)/2-legend_ysize/2 - delta2}
#     if (pos == "right") {xref <- x2 - 2*delta1 - legend_xsize ; yref <- (y1+y2)/2-legend_ysize/2 - delta2}
#     if (pos == "top") {xref <- (x1+x2)/2 - legend_xsize/2 ; yref <- y2 - 2*delta1 - legend_ysize}
#     if (pos == "bottom") {xref <- (x1+x2)/2 - legend_xsize/2 ; yref <- y1 + delta1}
#     if (pos == "middle") { xref <- (x1+x2)/2 - legend_xsize/2 ; yref <- (y1+y2)/2-legend_ysize/2 - delta2}
#     
#     
#     # Frame
#     if (frame==TRUE){
#       rect(xref-delta1, yref-delta1, xref+legend_xsize + delta1*2, yref+legend_ysize + delta1 *2, border = "black",  col="white")
#     }
#     
#     mycol <- col
#     
# 
#     
#     if (style=="c"){
#       # (V1)
#       
#       
#       for(i in 1:4){
#         
#         rect(xref, yref, xref + height/1.5, yref + rLeg[i] ,col=mycol)
#       }
#       
#       for(i in 1:4){
#         segments(xref + height/1.5 ,yref+rLeg[i],xref+height/1.5 + delta2,yref +rLeg[i])
#         text(xref+  height/1.5 + delta1 ,y= yref+rLeg[i],rVal[i],adj=c(0,0.5),cex=values.cex)
#       }
#       
#       
#       text(x=xref ,y=yref + rLeg[1] +delta2,title.txt,adj=c(0,0),cex=title.cex)
#       
#     }
#     if (style=="e"){
#       
#       #  (V2)
#       jump <- 0
#       for(i in 4:1){
#         rect(xref, yref+ jump, xref+height/1.5, yref+rLeg[i] + jump ,col=mycol)
#         #symbols(x = xref + rLeg[i]/2 + (rLeg[1]-rLeg[i])/2 ,y=yref + jump,squares=rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
#         text(xref + height/1.5 + delta2 ,y= yref + jump + rLeg[i]/2,rVal[i],adj=c(0,0.5),cex=values.cex)
#         if (i>1){jump <- rLeg[i] + delta2 + jump}
#       }
#       text(x=xref ,y=yref + rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + delta2 ,title.txt,adj=c(0,0),cex=title.cex)
#     }
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
