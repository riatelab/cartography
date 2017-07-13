# legpos <- function(pos, x1, x2, y1, y2, delta1, delta2, legend_xsize, legend_ysize){
#   # Position
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
# legendCirclesSymbols2 <- function(pos = "topleft", title.txt = "Title of the legend",
#                                  title.cex = 0.8, cex = 1,
#                                  values.cex = 0.6, var, inches, col = "red",
#                                  frame = FALSE, values.rnd = 0, style = "c"){
#   var <- abs(var)
#   # exit for none
#   positions <- c("bottomleft", "topleft", "topright", "bottomright", 
#                  "left", "right", "top", "bottom", "center")
#   if(!pos %in% positions){return()}
#   
#   # figdim in geo coordinates
#   x1 <- par()$usr[1]
#   x2 <- par()$usr[2]
#   y1 <- par()$usr[3]
#   y2 <- par()$usr[4]
#   
#   # offsets
#   delta1 <- xinch(0.1) * cex
#   delta2 <- delta1 / 2 
#   
#   # Create circles
#   ## with unknown intermediates values OR with a list of values
#   if(length(var) == 2){
#     siz <- sqrt((var * inches * inches * pi  / max(var)) / pi)
#     siz <- seq(from = max(siz), to = min(siz), length.out = 4)
#     sle <- siz * siz * pi
#     var <- sle * max(var) / sle[1]
#   }else{  
#     siz <- sqrt((var * inches * inches * pi  / max(var)) / pi)
#   }
#   size <- xinch(siz)
#   var <- round(var,values.rnd)
#   size <- sort(size, decreasing = T)  
#   var <- sort(var, decreasing = T)  
#   
#   
#   # Legend width and height
#   if(style == "c"){
#     longVal <- var[strwidth(var,cex = values.cex) == max(strwidth(var, cex = values.cex))][1]
#     legend_xsize <- max(size[1] * 2 + strwidth(longVal, cex = values.cex),
#                         strwidth(title.txt,cex = title.cex) - delta1)
#     legend_ysize <-size[1] * 2 + strheight(title.txt, cex = title.cex)
#   }
#   
#   # Get legend position
#   legcoord <- legpos(pos = pos, x1 = x1, x2 = x2, y1 = y1, y2 = y2, 
#                      delta1 = delta1, delta2 = delta2, 
#                      legend_xsize = legend_xsize, legend_ysize = legend_ysize)
#   xref <- legcoord$xref
#   yref <- legcoord$yref
#   
#   # Frame display
#   if(frame == TRUE){
#     rect(xref - delta1, yref - delta1, xref + legend_xsize + delta1 * 2, 
#          yref + legend_ysize + delta1 * 2, border = "black",  col="white")
#   }
#   
#   # legend display
#   if(style=="c"){
#     for(i in 1:length(size)){
#       symbols(x = xref + size[1], y = yref + size[i], circles = size[i],
#               add = TRUE, bg = col, inches = FALSE)
#     }
#     for(i in 1:length(var)){
#       segments(xref + size[1], yref + size[i] * 2, xref + size[1] * 2 + delta2,
#                yref + size[i] * 2)
#       text(x = xref + size[1] * 2 + delta1, y = yref + size[i] * 2, labels = var[i],
#            adj = c(0,0.5), cex = values.cex)
#     }
#     text(x = xref ,y = yref + delta2 + size[1] * 2 + delta2, title.txt, 
#          adj = c(0,0), cex = title.cex)
#   }
# }
# 
# 
# 
# 
# 
# library(cartography)
# options(scipen = 7)
# data("nuts2006")
# nf <- layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE),height = c(4/5,1/5))
# par(mar=c(0,0,0,0))
# 
# plot(nuts0.spdf)
# propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, var = "pop2008", 
#                  legend.pos = "bottomleft", inches = 0.3, col = "red")
# box()
# 
# 
# # il faut d'abord plotter (invisible) la couche ayant été utilisée pour les propSymbols
# plot(nuts0.spdf, col = NA, border = NA)
# ## Les cercles de la légendes sont choisis explicitement
# ## Le paramètre inches doit correspondre à celui utilisé dans propSymbolsLayer
# legendCirclesSymbols2(var=c(35500,5000000,20000000,40000000,82217837), values.cex = 0.9,
#                      inches = 0.3, pos = "center", col = "red", title.cex = 1.2,
#                      title.txt = "Population")
# box()
# 
# plot(nuts0.spdf, col = NA, border = NA)
# legendCirclesSymbols2(var=c(5000000,20000000,82217837), values.cex = 0.9,
#                      inches = 0.3, pos = "center", col = "red", title.cex = 1.2,
#                      title.txt = "Population")
# box()
# 
# 
# ## Les cercles de la légendes sont choisis en fonction des valeurs min et max fournies
# plot(nuts0.spdf, col = NA, border = NA)
# legendCirclesSymbols2(var=c(35356,82217837), 
#                      inches = 0.3, pos = "center", col = "red",
#                      title.txt = "Population")
# box()
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
