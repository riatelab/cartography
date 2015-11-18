#' @title Legend for Choropleth Maps
#' @description Plot legend for choropleth maps.
#' @name legendChoro
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param breaks break points in sorted order to indicate the intervals for assigning the colors. 
#' Note that if there are nlevel colors (classes) there should be (nlevel+1) breakpoints.
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
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf, col = "grey")
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
                        frame=FALSE,symbol="box"){
  
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
    
    paramsize1 = 30/cex
    paramsize2 <- paramsize1*40/25
    
    
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    #rect(x1, y1, x2, y2, border = "black")
    
    
    # Taille du bloc de legende
    breaks <- as.numeric(round(breaks, values.rnd))
    longVal <- breaks[strwidth(breaks,cex=values.cex)==max(strwidth(breaks,cex=values.cex))][1]
    if (nodata == TRUE){if (strwidth(nodata.txt,cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- nodata.txt}}
    legend_xsize <- max(width + strwidth(longVal,cex=values.cex),strwidth(title.txt,cex = title.cex) - delta2) - delta2
    legend_ysize <- (length(breaks)-1) * height +  strheight(title.txt,cex = title.cex)
    
    # legende_size augmente si un caisson no data
    if (nodata == TRUE){legend_ysize <- legend_ysize + height + delta2 }
    
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
      rect(xref-delta1, yref-delta1, xref+legend_xsize + delta1*2, yref+legend_ysize + delta1 * 2, border = "black",  col="white")
    }
    
    # Affichage du bloc de legende
    
    if (nodata == TRUE){
      rect(xref,yref ,xref + width,yref + height,col=nodata.col,border="black",lwd=0.4)
      text(xref + width + delta2 ,yref + height/2 ,nodata.txt,adj=c(0,0.5),cex=values.cex)
      yref <- yref + height + delta2
    }
    
    if (symbol=="line"){
      
      for (i in 0:(length(breaks)-2)){
        
        segments(xref, yref + height/2+ i*height, xref + width, yref + i*height + height/2, lwd=5, col=col[i+1], lend=1)
        
        #  rect(xref,yref + i*height,xref + width,yref + height + i*height,col=col[i+1],border="black",lwd=0.4)
      }
    } else { #box
      for (i in 0:(length(breaks)-2)){
        rect(xref,yref + i*height,xref + width,yref + height + i*height,col=col[i+1],border="black",lwd=0.4)
      }
      
    }
    
    # Affichage des textes
    for (i in 1:(length(breaks))){
      j <- i -1
      text(xref + width + delta2 ,y= yref + j * height,breaks[i],adj=c(0,0.5),cex=values.cex)
    }
    
    # Affichage du titre
    text(x=xref,y=yref + (length(breaks)-1)*height+delta1,title.txt,adj=c(0,0),cex=title.cex)
    
  }
  
}


#' @title  Legend for Typology Maps
#' @description Plot legend for typology maps.
#' @name legendTypo
#' @param categ vector of categories.
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
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
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf, col = "grey")
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
  categ <- rev(categ)
  col <- rev(col)
  positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "middle")
  if(pos %in% positions){
    
    x1 <- par()$usr[1]
    x2 <- par()$usr[2]
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    xextent <- x2 - x1
    yextent <- y2 - y1
    paramsize1 = 30/cex
    paramsize2 <- paramsize1*40/25
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    #rect(x1, y1, x2, y2, border = "black")
    
    
    # xsize
    longVal <- categ[strwidth(categ,cex=values.cex)==max(strwidth(categ,cex=values.cex))][1]
    if (nodata == TRUE){if (strwidth(nodata.txt,cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- nodata.txt}}
    
    legend_xsize <- max(width + delta1 + strwidth(longVal,cex=values.cex) ,strwidth(title.txt,cex = title.cex)) - delta1
    
    # ysize
    legend_ysize <- (length(categ)) * height + delta2 * (length(categ)) + strheight(title.txt,cex = title.cex) - delta2
    if (nodata == TRUE){legend_ysize <- legend_ysize + height + delta2 }
    
    
    
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
    
    
    if (nodata == TRUE){
      rect(xref,yref ,xref + width,yref + height,col=nodata.col,border="black",lwd=0.4)
      text(xref + width + delta2 ,yref + height/2 ,nodata.txt,adj=c(0,0.5),cex=values.cex)
      yref <- yref + height + delta2
    }
    
    
    if (symbol=="box"){
      for (i in 0:(length(categ)-1)){
        rect(xref,yref + i*height + i*delta2,xref + width,yref + height + i*height + i*delta2,col=col[i+1],border="black",lwd=0.4)
        j <- i+1
        text(xref + width + delta2 ,y= yref + height/2 + i * height + i*delta2,categ[j],adj=c(0,0.5),cex=values.cex)
        
      }
    }
    
    if (symbol=="line"){
      for (i in 0:(length(categ)-1)){
        
        segments(xref, yref + height/2+ i*height+i*delta2, xref + width, yref + i*height+i*delta2 + height/2, lwd=5, col=col[i+1], lend=1)
        #rect(xref,yref + i*height + i*delta2,xref + width,yref + height + i*height + i*delta2,col=col[i+1],border="black",lwd=0.4)
        j <- i+1
        text(xref + width + delta2 ,y= yref + height/2 + i * height + i*delta2,categ[j],adj=c(0,0.5),cex=values.cex)
        
      }
    }
    # Affichage du titre
    text(x=xref,y=yref + length(categ)*height + length(categ)*delta2 + delta2,title.txt,adj=c(0,0),cex=title.cex)
    
  }
}

#' @title Legend for Proportional Circles Maps
#' @description Plot legend for proportional circles maps
#' @name legendCirclesSymbols
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var vector of values.
#' @param r a vector giving the radii of the circles.
#' @param breakval breaking value (see Details).
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param col2 second color of symbols (see Details).
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @details The breakval parameter allows to plot symbols of two 
#' colors: the first color (col) for values superior or equal to breakval,
#' second color (col2) for values inferior to breakval.
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' 
#' legendCirclesSymbols(pos = "topleft", title.txt = "Title of\nthe legend",
#'                      title.cex = 0.8, values.cex = 0.6,cex = 1.5,
#'                      var = nuts1.df$pop2008,
#'                      r = sqrt((abs(nuts1.df$pop2008) * 100000) / pi),
#'                      col = "pink", frame = TRUE, 
#'                      values.rnd=0, style ="c")
legendCirclesSymbols<- function(pos = "topleft", title.txt = "Title of the legend", 
                                title.cex = 0.8, cex = 1,
                                values.cex = 0.6, var, r, breakval = NULL, col="red", 
                                col2="blue", frame=FALSE, values.rnd=0, style ="c"){
  var <- abs(var)
  
  positions <- c("bottomleft", "topleft", "topright", "bottomright", 
                 "left", "right", "top", "bottom", "middle")
  if(pos %in% positions){
    
    # extent
    x1 <- par()$usr[1]
    x2 <- par()$usr[2]
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    xextent <- x2 - x1
    yextent <- y2 - y1
    
    # variables internes
    paramsize1 = 30/cex
    paramsize2 <- paramsize1 * 40 / 25
    width <- xextent / paramsize1
    height <- width / 1.5
    delta1 <- min(yextent / paramsize2, xextent / paramsize2) # Gros eccart entre les objets
    delta2 <- delta1 / 2 # Petit eccart entre les objets
    
    rValmax <- max(var,na.rm = TRUE)
    rValmin <- min(var,na.rm = TRUE)
    rValextent <- rValmax - rValmin
    rLegmax <- max(r,na.rm = TRUE)
    rLegmin <- min(r,na.rm = TRUE)
    rLegextent <- rLegmax - rLegmin
    
    # rVal <- c(rValmax,rValmax - rValextent/3 , rValmax - 2*(rValextent/3),rValmin)
    #     rLeg <- c(rLegmax, 
    #               rLegmax - rLegextent/3, 
    #               rLegmax - 2*(rLegextent/3), 
    #               rLegmin)
    rLeg <- seq(from = rLegmax, to = rLegmin, length.out = 4)
    
    sleg <- rLeg * rLeg * pi
    rVal <- sleg * rValmax / sleg[1]
    rVal <- round(rVal,values.rnd)
    
    
    # xsize & ysize
    
    if (style=="c"){
      longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
      if(!is.null(breakval)){
        if (strwidth(paste(">=",breakval),cex=values.cex)>strwidth(longVal,cex=values.cex)){
          longVal <- paste(">=",breakval)}
      }
      legend_xsize <- max(rLeg[1]*2 + strwidth(longVal,cex=values.cex),
                          strwidth(title.txt,cex = title.cex)-delta1)
      
      legend_ysize <-rLeg[1]*2 + strheight(title.txt,cex = title.cex)
      if(!is.null(breakval)){
        legend_ysize <- legend_ysize + height*2+ delta2
      }
    }
    
    
    if (style=="e"){
      longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
      if(!is.null(breakval)){
        if (strwidth(paste(">=",breakval),cex=values.cex)>strwidth(longVal,cex=values.cex)){
          longVal <- paste(">=",breakval)}
      }
      legend_xsize <- max(rLeg[1]*2 + strwidth(longVal,cex=values.cex),
                          strwidth(title.txt,cex = title.cex)-delta1)
      
      legend_ysize <-(rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4])*2 + 3*delta2 / 2 + strheight(title.txt,cex = title.cex)
      if(!is.null(breakval)){
        legend_ysize <- legend_ysize + height*2+ delta2
      }
    }
    
    
    # Position
    if (pos == "bottomleft") {
      xref <- x1 + delta1 
      yref <- y1 + delta1
    }
    if (pos == "topleft") {
      xref <- x1 + delta1 
      yref <- y2 - 2*delta1 - legend_ysize
    }
    if (pos == "topright") {
      xref <- x2 - 2*delta1 - legend_xsize 
      yref <- y2 -2*delta1 - legend_ysize
    }
    if (pos == "bottomright") {
      xref <- x2 - 2*delta1 - legend_xsize 
      yref <- y1 + delta1
    }
    if (pos == "left") {
      xref <- x1 + delta1 
      yref <- (y1+y2)/2-legend_ysize/2 - delta2
    }
    if (pos == "right") {
      xref <- x2 - 2*delta1 - legend_xsize 
      yref <- (y1+y2)/2-legend_ysize/2 - delta2
    }
    if (pos == "top") {
      xref <- (x1+x2)/2 - legend_xsize/2 
      yref <- y2 - 2*delta1 - legend_ysize
    }
    if (pos == "bottom") {
      xref <- (x1+x2)/2 - legend_xsize/2 
      yref <- y1 + delta1
    }
    if (pos == "middle") {
      xref <- (x1+x2)/2 - legend_xsize/2 
      yref <- (y1+y2)/2-legend_ysize/2 - delta2
    }
    
    
    # Frame
    if (frame==TRUE){
      rect(xref-delta1, 
           yref-delta1, 
           xref+legend_xsize + delta1*2, 
           yref+legend_ysize + delta1 *2, 
           border = "black",  col="white")
    }
    
    mycol <- col
    
    if(!is.null(breakval)){
      
      symbols(x = xref + rLeg[1] ,
              y=yref + delta1 - height/2 + height,
              circles=height/3,
              add = TRUE, bg = col, inches = FALSE)
      symbols(x = xref + rLeg[1] ,
              y=yref + delta1 - height/2,
              circles=height/3,
              add = TRUE, bg= col2, inches = FALSE)
      text(xref + rLeg[1] + height/3 + delta2, 
           yref + height/3 ,
           paste ("<",format(breakval,scientific=FALSE)),
           adj=c(0,0.5),cex=values.cex)
      text(xref + rLeg[1] + height/3 + delta2,
           yref + height+ height/3,
           paste (">=",format(breakval,scientific=FALSE)),
           adj=c(0,0.5),cex=values.cex)
      
      yref <- yref + height *2 + delta2
      mycol <- "white"
    }
    
    
    if (style=="c"){
      # cercles (V1)
      

      for(i in 1:4){
        symbols(x = xref + rLeg[1],
                y = yref + rLeg[i],
                circles = rLeg[i],
                add = TRUE,
                bg = mycol,
                inches = FALSE)
      }
      for(i in 1:4){
        
        segments(xref + rLeg[1],
                 yref + rLeg[i] * 2,
                 xref + rLeg[1] * 2 + delta2,
                 yref + rLeg[i] * 2)
        text(x = xref + rLeg[1] * 2 + delta1,
             y = yref + rLeg[i] * 2,
             labels = rVal[i],
             adj = c(0,0.5),
             cex = values.cex)
      }
      
      
      text(x=xref ,y=yref + delta2 + rLeg[1]*2 + delta2,title.txt,adj=c(0,0),cex=title.cex)
      
    }
    if (style=="e"){
      
      # cercles (V2)
      jump <- 0
      for(i in 4:1){
        symbols(x = xref + rLeg[1],y=yref + rLeg[i] + jump,circles=rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
        jump <- jump + rLeg[i]*2 + delta2/2
      }
      
      jump <- 0
      for(i in 4:1){
        text(xref+rLeg[1]+ rLeg[i] + delta2 ,y= yref+rLeg[i] + jump,rVal[i],adj=c(0,0.5),cex=values.cex)
        jump <- jump + rLeg[i]*2 + delta2/2
      }
      text(x=xref ,y=yref + (rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4])*2 + 3*delta2/2 + delta1,title.txt,adj=c(0,0),cex=title.cex)
    }
    
  }
  
}



#' @title Legend for Proportional Squares Maps
#' @description Plot legend for proportional squares maps
#' @name legendSquaresSymbols
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var vector of values.
#' @param r a vector giving the length of the sides of the squares.
#' @param breakval breaking value (see Details).
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param col2 second color of symbols (see Details).
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @details The breakval parameter allows to plot symbols of two 
#' colors: the first color (col) for values superior or equal to breakval,
#' second color (col2) for values inferior to breakval.
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
#' legendSquaresSymbols(pos = "bottomright", title.txt = "Title of\nthe legend ",
#'                      title.cex = 0.8, values.cex = 0.6,
#'                      var = nuts1.df$pop2008,
#'                      r = sqrt((abs(nuts1.df$pop2008) * 5000)),
#'                      breakval=10, col="red", col2="blue", 
#'                      frame=TRUE, values.rnd=0, style ="c")
#'
legendSquaresSymbols<- function(pos = "topleft", title.txt = "Title of the legend", 
                                title.cex = 0.8, cex = 1,
                                values.cex = 0.6, var, r, breakval = NULL, 
                                col="red", col2="blue", frame=FALSE, values.rnd=0, style ="c"){
  
  var <- abs(var)
  positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "middle")
  if(pos %in% positions){
    
    # extent
    x1 <- par()$usr[1]
    x2 <- par()$usr[2]
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    xextent <- x2 - x1
    yextent <- y2 - y1
    
    # variables internes
    paramsize1 = 30/cex
    paramsize2 <- paramsize1*40/25
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    
    
    rValmax <- max(var,na.rm = TRUE)
    rValmin <- min(var,na.rm = TRUE)
    rValextent <- rValmax - rValmin
    rLegmax <- max(r,na.rm = TRUE)
    rLegmin <- min(r,na.rm = TRUE)
    rLegextent <- rLegmax - rLegmin
    
    # rVal <- c(rValmax,rValmax - rValextent/3 , rValmax - 2*(rValextent/3),rValmin)
    rLeg <- c(rLegmax,rLegmax - rLegextent/3 , rLegmax - 2*(rLegextent/3),rLegmin)
    
    sleg <- rLeg * rLeg
    rVal <- sleg * rValmax / sleg[1]
    rVal <- round(rVal,values.rnd)
    
    
    # xsize & ysize
    
    if (style=="c"){
      longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
      if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- paste(">=",breakval)}}
      legend_xsize <- max(rLeg[1] + strwidth(longVal,cex=values.cex),strwidth(title.txt,cex = title.cex)-delta1)
      
      legend_ysize <-rLeg[1] + strheight(title.txt,cex = title.cex)
      if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2+ delta2}
    }
    
    if (style=="e"){
      longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
      if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- paste(">=",breakval)}}
      legend_xsize <- max(rLeg[1] + strwidth(longVal,cex=values.cex)+ delta2,strwidth(title.txt,cex = title.cex))-delta1
      
      legend_ysize <-rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + strheight(title.txt,cex = title.cex)
      if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2+ delta2}
    }
    
    
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
    
    mycol <- col
    
    if(!is.null(breakval)){
      
      
      if (style=="c"){
        symbols(x = xref+height/3,y=yref + delta1 - height/2 + height,squares=height/1.5,add=TRUE,bg=col,inches=FALSE)
        symbols(x = xref+height/3 ,y=yref + delta1 - height/2,squares=height/1.5,add=TRUE,bg=col2,inches=FALSE)
        text(xref + height/1.5 + delta2,yref + height/4,paste ("<",format(breakval,scientific=FALSE)),adj=c(0,0.5),cex=values.cex)
        text(xref + height/1.5 + delta2 , yref + height + height/4,paste (">=",format(breakval,scientific=FALSE)),adj=c(0,0.5),cex=values.cex)
        yref <- yref + height *2 + delta2
      }
      if (style == "e"){
        symbols(x = xref + rLeg[1]/2,y=yref + delta1 - height/2 + height,squares=height/1.5,add=TRUE,bg=col,inches=FALSE)
        symbols(x = xref + rLeg[1]/2 ,y=yref + delta1 - height/2,squares=height/1.5,add=TRUE,bg=col2,inches=FALSE)
        text(xref + rLeg[1]/2 + height/3 + delta2,yref + height/4,paste ("<",format(breakval,scientific=FALSE)),adj=c(0,0.5),cex=values.cex)
        text(xref + rLeg[1]/2 + height/3 + delta2 , yref + height + height/4,paste (">=",format(breakval,scientific=FALSE)),adj=c(0,0.5),cex=values.cex)
        yref <- yref + height *2 + delta2
      }
      
      mycol <- "white"
    }
    
    
    if (style=="c"){
      # squares (V1)
      for(i in 1:4){
        symbols(x = xref + rLeg[i]/2 + (rLeg[1]-rLeg[i]),y=yref + rLeg[i]/2, squares = rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
      }
      
      for(i in 1:4){
        segments(xref+rLeg[1],yref+rLeg[i],xref+rLeg[1]+delta2,yref +rLeg[i])
        text(xref+rLeg[1] + delta1 ,y= yref+rLeg[i],rVal[i],adj=c(0,0.5),cex=values.cex)
      }
      
      
      text(x=xref ,y=yref + rLeg[1] + delta1 ,title.txt,adj=c(0,0),cex=title.cex)
      
    }
    if (style=="e"){
      
      # cercles (V2)
      jump <- rLeg[4]/2
      for(i in 4:1){
        symbols(x = xref + rLeg[i]/2 + (rLeg[1]-rLeg[i])/2 ,y=yref + jump,squares=rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
        text(xref + rLeg[i] + (rLeg[1]-rLeg[i])/2 + delta2 ,y= yref + jump,rVal[i],adj=c(0,0.5),cex=values.cex)
        if (i>1){jump <- rLeg[i]/2 +  rLeg[i-1]/2 + jump  + delta2}
      }
      text(x=xref ,y=yref + rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + delta1 ,title.txt,adj=c(0,0),cex=title.cex)
    }
  }
}



#' @title Legend for Proportional Bars Maps
#' @description Plot legend for proportional bars maps
#' @name legendBarsSymbols
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var vector of values.
#' @param r a vector giving the heights of the bars.
#' @param breakval breaking value (see Details).
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param col2 second color of symbols (see Details).
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param style either "c" or "e". The legend has two display 
#' styles, "c" stands for compact and "e" for extended.
#' @details The breakval parameter allows to plot symbols of two 
#' colors: the first color (col) for values superior or equal to breakval,
#' second color (col2) for values inferior to breakval.
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' 
#' legendBarsSymbols(pos = "topleft", title.txt = "Title of\nthe legend",
#'                      title.cex = 0.8, values.cex = 0.6,cex = 3,
#'                      var = nuts1.df$pop2008,
#'                      r = sqrt((abs(nuts1.df$pop2008) * 100000) / pi),
#'                      col = "purple",
#'                      values.rnd=0, style ="e")
legendBarsSymbols<- function(pos = "topleft", title.txt = "Title of the legend", 
                             title.cex = 0.8, cex = 1,
                             values.cex = 0.6, var, r, breakval = NULL, 
                             col="red", col2="blue", frame=FALSE, values.rnd=0, style ="c"){
  
  var <- abs(var)
  positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "middle")
  if(pos %in% positions){
    
    # extent
    x1 <- par()$usr[1]
    x2 <- par()$usr[2]
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    xextent <- x2 - x1
    yextent <- y2 - y1
    
    # variables internes
    paramsize1 = 30/cex
    paramsize2 <- paramsize1*40/25
    width <- (x2 - x1) / 40
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    
    
    rValmax <- max(var,na.rm = TRUE)
    rValmin <- min(var,na.rm = TRUE)
    rValextent <- rValmax - rValmin
    rLegmax <- max(r,na.rm = TRUE)
    rLegmin <- min(r,na.rm = TRUE)
    rLegextent <- rLegmax - rLegmin
    
    rVal <- c(rValmax,rValmax - rValextent/3 , rValmax - 2*(rValextent/3),rValmin)
    rLeg <- c(rLegmax,rLegmax - rLegextent/3 , rLegmax - 2*(rLegextent/3),rLegmin)
    rVal <- round(rVal,values.rnd)
    
    
    # xsize & ysize
    
    if (style=="c"){
      longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
      if(!is.null(breakval)){if (strwidth(paste (">=",format(breakval,scientific=FALSE)),cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- paste (">=",format(breakval,scientific=FALSE))}}
      legend_xsize <- max(height/1.5 + strwidth(longVal,cex=values.cex),strwidth(title.txt,cex = title.cex)-delta1)
      
      legend_ysize <-rLeg[1] + strheight(title.txt,cex = title.cex) - delta1
      if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2}
    }
    
    if (style=="e"){
      longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
      if(!is.null(breakval)){if (strwidth(paste (">=",format(breakval,scientific=FALSE)),cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <-paste (">=",format(breakval,scientific=FALSE))}}
      legend_xsize <- max(height/1.5 + strwidth(longVal,cex=values.cex)-delta2,strwidth(title.txt,cex = title.cex)-delta1)
      
      legend_ysize <-rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + strheight(title.txt,cex = title.cex)- delta1
      if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2}
    }
    
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
    
    mycol <- col
    
    if(!is.null(breakval)){
      
      yref <- yref - delta1  
      symbols(x = xref + height/3  ,y=yref + delta1 - height/2 + height,squares=height/1.5,add=TRUE,bg=col,inches=FALSE)
      symbols(x = xref + height/3  ,y=yref + delta1 - height/2,squares=height/1.5,add=TRUE,bg=col2,inches=FALSE)
      text(xref + height/1.5 + delta2,yref + delta1 - height/3 ,paste ("<",format(breakval,scientific=FALSE)),adj=c(0,0.5),cex=values.cex)
      text(xref + height/1.5 + delta2, yref + height + delta1 - height/3 ,paste (">=",format(breakval,scientific=FALSE)),adj=c(0,0.5),cex=values.cex)
      yref <- yref + height *2 + delta1
      mycol <- "white"
    }
    
    
    if (style=="c"){
      # (V1)
      
      
      for(i in 1:4){
        
        rect(xref, yref, xref+height/1.5, yref+rLeg[i] ,col=mycol)
      }
      
      for(i in 1:4){
        segments(xref + height/1.5 ,yref+rLeg[i],xref+height/1.5 + delta2,yref +rLeg[i])
        text(xref+  height/1.5 + delta1 ,y= yref+rLeg[i],rVal[i],adj=c(0,0.5),cex=values.cex)
      }
      
      
      text(x=xref ,y=yref + rLeg[1] +delta2,title.txt,adj=c(0,0),cex=title.cex)
      
    }
    if (style=="e"){
      
      #  (V2)
      jump <- 0
      for(i in 4:1){
        rect(xref, yref+ jump, xref+height/1.5, yref+rLeg[i] + jump ,col=mycol)
        #symbols(x = xref + rLeg[i]/2 + (rLeg[1]-rLeg[i])/2 ,y=yref + jump,squares=rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
        text(xref + height/1.5 + delta2 ,y= yref + jump + rLeg[i]/2,rVal[i],adj=c(0,0.5),cex=values.cex)
        if (i>1){jump <- rLeg[i] + delta2 + jump}
      }
      text(x=xref ,y=yref + rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + delta2 ,title.txt,adj=c(0,0),cex=title.cex)
    }
  }
}

#' @title Legend for Double Proportional Triangles Maps
#' @description Plot legends for double proportional triangles maps.
#' @name legendPropTriangles
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
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
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' box()
#' var <- round((nuts0.df$pop2008 / sum(nuts0.df$pop2008))*100,2)
#' var2 <- round((nuts0.df$gdppps2008 / sum(nuts0.df$gdppps2008))*100,2)
#' r <- sqrt(var)/2*1000000
#' r2 <- sqrt(var2)/2*1000000
#' legendPropTriangles(pos = "topright", var.txt = "population totale (habs)",
#'                         var2.txt = "pib (euros)", title.txt="PIB par habitant",
#'                         title.cex = 0.8, values.cex = 0.6, cex = 1,
#'                         var = var, var2 = var2, r = r, r2 = r2,
#'                         col="green", col2="yellow", frame=TRUE, values.rnd=2, 
#'                         style="c")
legendPropTriangles<- function(pos = "topleft", title.txt, var.txt,var2.txt, 
                               title.cex = 0.8, cex = 1,
                               values.cex = 0.6, var, var2, r, r2, col="red", 
                               col2="blue", frame=FALSE, values.rnd=0, 
                               style="c"){
  
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
    paramsize1 = 30/cex
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
      
      mysize <- (rLegmax2 - rLegmin2)/2 ;
      polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref+yadd), col = col2)
      segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
      text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,round(rVal2[2],values.rnd),cex=values.cex,adj=c(0,0.5))
      
      mysize <- rLegmin2 ;
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


#' @title Legend for Proportional Lines Maps
#' @description Plot legend for proportional lines maps
#' @name legendPropLines
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param var vector of values.
#' @param lwd a vector giving the width of the lines.
#' @param values.rnd number of decimal places of the values in 
#' the legend.
#' @param col color of symbols.
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' box()
#' legendPropLines(pos = "topleft", title.txt = "Title",
#'                 title.cex = 0.8, values.cex = 0.6, cex = 2,
#'                 var = nuts1.df$pop2008,
#'                 lwd = nuts1.df$pop2008/1000000,
#'                 col="red", frame=TRUE, values.rnd=2)
legendPropLines<- function(pos = "topleft", title.txt = "Title of the legend", 
                           title.cex = 0.8, cex = 1,
                           values.cex = 0.6, var, lwd, col="red", frame=FALSE, 
                           values.rnd=0){
  positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "middle")
  if(pos %in% positions){
    # extent
    x1 <- par()$usr[1]
    x2 <- par()$usr[2]
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    xextent <- x2 - x1
    yextent <- y2 - y1
    
    # variables internes
    paramsize1 = 30/cex
    paramsize2 <- paramsize1*40/25
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    
    
    rValmax <- max(var,na.rm = TRUE)
    rValmin <- min(var,na.rm = TRUE)
    rValextent <- rValmax - rValmin
    rLegmax <- max(lwd,na.rm = TRUE)
    rLegmin <- min(lwd,na.rm = TRUE)
    rLegextent <- rLegmax - rLegmin
    
    rVal <- c(rValmax,rValmax - rValextent/3 , rValmax - 2*(rValextent/3),rValmin)
    rLeg <- c(rLegmax,rLegmax - rLegextent/3 , rLegmax - 2*(rLegextent/3),rLegmin)
    rVal <- round(rVal,values.rnd)
    
    # xsize & ysize
    
    longVal <- rVal[strwidth(rVal,cex=values.cex)==max(strwidth(rVal,cex=values.cex))][1]
    #if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- paste(">=",breakval)}}
    legend_xsize <- max(width+ strwidth(longVal,cex=values.cex)-delta2,strwidth(title.txt,cex = title.cex)-delta1)
    
    legend_ysize <-8*delta1 + strheight(title.txt,cex = title.cex)
    
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
    
    mycol <- col
    
    jump <- delta1
    for(i in 4:1){
      
      if (rLeg[i] < 0.2){rLeg[i] <- 0.2} # TAILLE DES LIGNE MINIMALES (A METTRE AUSSI SUR LES CARTES)
      
      segments(xref, yref + jump, xref + width, yref + jump, col=mycol, lwd=rLeg[i],lend=1)
      text(xref + width + delta2 ,y= yref + jump,rVal[i],adj=c(0,0.5),cex=values.cex)
      jump <- jump + 2*delta1 # ICI AMELIORER
    }
    text(x=xref ,y=yref + 9*delta1,title.txt,adj=c(0,0),cex=title.cex)
  }
}

#' @title Legend for Graduated Size Lines Maps
#' @description Plot legend for graduated size lines maps.
#' @name legendGradLines
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright".
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
#' @examples 
#' data("nuts2006")
#' plot(nuts0.spdf)
#' box()
#' legendGradLines(title.txt = "Title of the legend", 
#'                 pos = "topright",
#'                 title.cex = 0.8,
#'                 values.cex = 0.6, breaks = c(1,2,3,4,10.2,15.2),
#'                 lwd = c(0.2,2,4,5,10),
#'                 col ="blue", values.rnd =2)
legendGradLines <- function(pos = "topleft", title.txt = "Title of the legend", 
                            title.cex = 0.8,
                            values.cex = 0.6, breaks, lwd, col, values.rnd =2, 
                            cex = 1,
                            frame=FALSE){
  
  positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "middle")
  if(pos %in% positions){
    # extent
    x1 <- par()$usr[1]
    x2 <- par()$usr[2]
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    xextent <- x2 - x1
    yextent <- y2 - y1
    
    
    # variables internes
    paramsize1 = 30/cex
    paramsize2 <- paramsize1*40/25
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    #rect(x1, y1, x2, y2, border = "black")
    
    
    # Taille du bloc de legende
    breaks <- as.numeric(round(breaks, values.rnd))
    longVal <- breaks[strwidth(breaks,cex=values.cex)==max(strwidth(breaks,cex=values.cex))][1]
    # if (nodata == TRUE){if (strwidth(nodata.txt,cex=values.cex)>strwidth(longVal,cex=values.cex)){longVal <- nodata.txt}}
    legend_xsize <- max(width + strwidth(longVal,cex=values.cex),strwidth(title.txt,cex = title.cex) - delta2) - delta2
    legend_ysize <- length(breaks)*height + (length(breaks)-2)*delta2 +  strheight(title.txt,cex = title.cex)
    
    # legende_size augmente si un caisson no data
    # if (nodata == TRUE){legend_ysize <- legend_ysize + height + delta2 }
    
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
      rect(xref-delta1, yref-delta1, xref+legend_xsize + delta1*2, yref+legend_ysize + delta1 * 2, border = "black",  col="white")
    }
    
    # Affichage du bloc de legende
    
    #     if (nodata == TRUE){
    #       rect(xref,yref ,xref + width,yref + height,col=nodata.col,border="black",lwd=0.4)
    #       text(xref + width + delta2 ,yref + height/2 ,nodata.txt,adj=c(0,0.5),cex=values.cex)
    #       yref <- yref + height + delta2
    #     }
    
    for (i in 0:(length(breaks)-2)){
      j <- i+1
      segments(xref, yref + height/2+ i*height+i*delta2 + (height+delta2)/2, xref + width, yref + i*height+i*delta2 + height/2+ (height+delta2)/2, lwd=lwd[j], col=col, lend=1)
      #rect(xref,yref + i*height + i*delta2,xref + width,yref + height + i*height + i*delta2,col=col[i+1],border="black",lwd=0.4)
      text(xref + width + delta2 ,y= yref + height/2 + i * height + i*delta2,breaks[j],adj=c(0,0.5),cex=values.cex)
    }
    text(xref + width + delta2 ,y= yref + height/2 + (i+1)* height + (i+1)*delta2,breaks[j+1],adj=c(0,0.5),cex=values.cex)
    
    
    # Affichage du titre
    text(x=xref,y=yref + length(breaks)*height + length(breaks)*delta2,title.txt,adj=c(0,0),cex=title.cex)
    
  }
  
}
