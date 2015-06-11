# #####################################
# LEGENDES POUR CARTES CHOROPLETHES
# #####################################


#' @title LegendChoro
#' @description legend for chorpoleth layers
#' @name LegendChoro
#' @details plot a legend
#' @param pos bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle
#' @param legTitle Title of the legend
#' @param legTitleCex legTitleCex
#' @param legValuesCex legValuesCex
#' @param distr vector of classes
#' @param cols vector of colors
#' @param round round class values
#' @param nodata if TRUE, a white box 'no data' is drawn.
#' @param nodatalabel label for no data value. Default = "No data"
#' @param frame if TRUE, a frame is drawn
#' @param symbol 'line' or 'box'
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
#' LegendChoro(pos = "bottomright", legTitle = "Title of the legend", legTitleCex = 0.8,
#'              legValuesCex = 0.6, distr = c(1,2,3,4,10.27,15.2),
#'              cols = carto.pal(pal1 = "orange.pal",n1 = 5), round =2,
#'              nodata = TRUE, nodatalabel = "No data available", frame = TRUE, symbol="box")
#'  LegendChoro(pos = "bottomleft", legTitle = "Title of the legend", legTitleCex = 0.8,
#'              legValuesCex = 0.6, distr = c(1,2,3,4,10,15.27),
#'              cols = carto.pal(pal1 = "orange.pal",n1 = 5), round =2,
#'              nodata = TRUE, nodatalabel = "No data available",
#'              frame = TRUE, symbol="line")
#' @return plot


LegendChoro <- function(pos = "topleft", legTitle = "Title of the legend", legTitleCex = 0.8,
                        legValuesCex = 0.6, distr, cols, round =2, nodata = TRUE, nodatalabel = "No data", frame=FALSE,symbol="box"){

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
paramsize1 <- 25
paramsize2 <- 40
width <- (x2 - x1) / paramsize1
height <- width /1.5
delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
#rect(x1, y1, x2, y2, border = "black")


# Taille du bloc de legende
distr <- as.numeric(round(distr, round))
longVal <- distr[strwidth(distr,cex=legValuesCex)==max(strwidth(distr,cex=legValuesCex))][1]
if (nodata == TRUE){if (strwidth(nodatalabel,cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- nodatalabel}}
legend_xsize <- max(width + strwidth(longVal,cex=legValuesCex),strwidth(legTitle,cex = legTitleCex) - delta2) - delta2
legend_ysize <- (length(distr)-1) * height +  strheight(legTitle,cex = legTitleCex)

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
rect(xref,yref ,xref + width,yref + height,col="white",border="black",lwd=0.4)
text(xref + width + delta2 ,yref + height/2 ,nodatalabel,adj=c(0,0.5),cex=legValuesCex)
yref <- yref + height + delta2
}


if (symbol=="line"){

for (i in 0:(length(distr)-2)){

  segments(xref, yref + height/2+ i*height, xref + width, yref + i*height + height/2, lwd=5, col=cols[i+1], lend=1)

#  rect(xref,yref + i*height,xref + width,yref + height + i*height,col=cols[i+1],border="black",lwd=0.4)
}
} else { #box
  for (i in 0:(length(distr)-2)){
    rect(xref,yref + i*height,xref + width,yref + height + i*height,col=cols[i+1],border="black",lwd=0.4)
  }

}




# Affichage des textes
for (i in 1:(length(distr))){
  j <- i -1
  text(xref + width + delta2 ,y= yref + j * height,distr[i],adj=c(0,0.5),cex=legValuesCex)
}

# Affichage du titre
text(x=xref,y=yref + (length(distr)-1)*height+delta1,legTitle,adj=c(0,0),cex=legTitleCex)

}

}

# #########################
# LEGENDES POUR CARTES TYPO
# #########################


#' @title LegendTypo
#' @description legend for quali layers
#' @name LegendTypo
#' @details plot a legend
#' @param pos bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle
#' @param legTitle Title of the legend
#' @param legTitleCex legTitleCex
#' @param legValuesCex legValuesCex
#' @param categ factor of categories
#' @param cols vector of colors
#' @param nodata if TRUE, a white box 'no data' is drawn.
#' @param nodatalabel label for no data value. Default = "No data"
#' @param frame if TRUE, a frame is drawn
#' @param symbol 'line' or 'box'
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
#' LegendTypo(pos = "topright", legTitle = "Title of the legend, year",
#'            legTitleCex = 0.8, legValuesCex = 0.6,
#'            categ = c("John coltrane","Miles Davis","Maceo Parker",
#'            "Herbie\nHancock","Julien Lourau"),
#'            cols = c("red","blue","green","yellow","purple"),
#'            nodatalabel = "Absence\nd'information",nodata = TRUE,
#'            frame=TRUE,symbol="box")
#'
#' LegendTypo(pos = "right", legTitle = "Title of the legend, year",
#'            legTitleCex = 0.8, legValuesCex = 0.6,
#'            categ = c("John coltrane","Miles Davis","Maceo Parker",
#'            "Herbie\nHancock","Julien Lourau"),
#'            cols = c("red","blue","green","yellow","purple"),
#'            nodatalabel = "Absence\nd'information",nodata = TRUE,
#'            frame=TRUE, symbol="line")
#' @return plot


# #########################
# LEGENDES POUR CARTES TYPO
# #########################

LegendTypo <- function(pos = "bottomleft", legTitle = "Title of the legend", legTitleCex = 0.8,
                       legValuesCex = 0.6, categ, cols, nodata = TRUE, nodatalabel = "No data", frame=FALSE, symbol="box"){

  positions <- c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "middle")
  if(pos %in% positions){

    x1 <- par()$usr[1]
    x2 <- par()$usr[2]
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    xextent <- x2 - x1
    yextent <- y2 - y1
    paramsize1 <- 25
    paramsize2 <- 40
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    #rect(x1, y1, x2, y2, border = "black")


    # xsize
    longVal <- categ[strwidth(categ,cex=legValuesCex)==max(strwidth(categ,cex=legValuesCex))][1]
    if (nodata == TRUE){if (strwidth(nodatalabel,cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- nodatalabel}}

    legend_xsize <- max(width + delta1 + strwidth(longVal,cex=legValuesCex) ,strwidth(legTitle,cex = legTitleCex)) - delta1

    # ysize
    legend_ysize <- (length(categ)) * height + delta2 * (length(categ)) + strheight(legTitle,cex = legTitleCex) - delta2
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
      rect(xref,yref ,xref + width,yref + height,col="white",border="black",lwd=0.4)
      text(xref + width + delta2 ,yref + height/2 ,nodatalabel,adj=c(0,0.5),cex=legValuesCex)
      yref <- yref + height + delta2
    }


    if (symbol=="box"){
      for (i in 0:(length(categ)-1)){
        rect(xref,yref + i*height + i*delta2,xref + width,yref + height + i*height + i*delta2,col=cols[i+1],border="black",lwd=0.4)
        j <- i+1
        text(xref + width + delta2 ,y= yref + height/2 + i * height + i*delta2,categ[j],adj=c(0,0.5),cex=legValuesCex)

      }
    }

    if (symbol=="line"){
      for (i in 0:(length(categ)-1)){

        segments(xref, yref + height/2+ i*height+i*delta2, xref + width, yref + i*height+i*delta2 + height/2, lwd=5, col=cols[i+1], lend=1)
        #rect(xref,yref + i*height + i*delta2,xref + width,yref + height + i*height + i*delta2,col=cols[i+1],border="black",lwd=0.4)
        j <- i+1
        text(xref + width + delta2 ,y= yref + height/2 + i * height + i*delta2,categ[j],adj=c(0,0.5),cex=legValuesCex)

      }
    }
    # Affichage du titre
    text(x=xref,y=yref + length(categ)*height + length(categ)*delta2 + delta2,legTitle,adj=c(0,0),cex=legTitleCex)

  }
}


# #######################################
# LEGENDES POUR CERCLES PROPORTIONNELS
# #######################################

#' @title LegendCircSymbols
#' @description legend for PropCircles layers
#' @name LegendCircSymbols
#' @details plot a legend
#' @param pos bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle
#' @param legTitle Title of the legend
#' @param legTitleCex legTitleCex
#' @param legValuesCex legValuesCex
#' @param varvect Values
#' @param sizevect Sizes of the circles (r)
#' @param breakval breakval
#' @param round round
#' @param col1 color defalul = "red"
#' @param col2 color default = "blue"
#' @param frame if TRUE, a frame is drawn
#' @param type Type of display of the legend. "a" or "b"
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
#' LegendCircSymbols(pos = "bottomleft", legTitle = "Title of\nthe legend",
#'                   legTitleCex = 0.8, legValuesCex = 0.6,
#'                   varvect = nuts1.df$pop2008,
#'                   sizevect = sqrt((abs(nuts1.df$pop2008) * 100000) / pi),
#'                   breakval=1, col1="red", col2="blue", frame=TRUE, round=0, type ="a")
#'
#' LegendCircSymbols(pos = "topleft", legTitle = "Title of\nthe legend",
#'                   legTitleCex = 0.8, legValuesCex = 0.6,
#'                   varvect = nuts1.df$pop2008,
#'                   sizevect = sqrt((abs(nuts1.df$pop2008) * 1000) / pi),
#'                   breakval=1000, col1="red", col2="blue", frame=TRUE, round=0, type ="b")
#' @return plot


LegendCircSymbols<- function(pos = "topleft", legTitle = "Title of the legend", legTitleCex = 0.8,
                       legValuesCex = 0.6, varvect, sizevect, breakval, col1="red", col2="blue", frame=FALSE, round=0, type ="a"){

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
paramsize1 <- 25
paramsize2 <- 40
width <- (x2 - x1) / paramsize1
height <- width /1.5
delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
#rect(x1, y1, x2, y2, border = "black")

rValmax <- max(varvect,na.rm = TRUE)
rValmin <- min(varvect,na.rm = TRUE)
rValextent <- rValmax - rValmin
rLegmax <- max(sizevect,na.rm = TRUE)
rLegmin <- min(sizevect,na.rm = TRUE)
rLegextent <- rLegmax - rLegmin

rVal <- c(rValmax,rValmax - rValextent/3 , rValmax - 2*(rValextent/3),rValmin)
rLeg <- c(rLegmax,rLegmax - rLegextent/3 , rLegmax - 2*(rLegextent/3),rLegmin)
rVal <- round(rVal,round)





# xsize & ysize

if (type=="a"){
  longVal <- rVal[strwidth(rVal,cex=legValuesCex)==max(strwidth(rVal,cex=legValuesCex))][1]
  if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- paste(">=",breakval)}}
  legend_xsize <- max(rLeg[1]*2 + strwidth(longVal,cex=legValuesCex),strwidth(legTitle,cex = legTitleCex)-delta1)

  legend_ysize <-rLeg[1]*2 + strheight(legTitle,cex = legTitleCex)
  if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2+ delta2}
}

if (type=="b"){
  longVal <- rVal[strwidth(rVal,cex=legValuesCex)==max(strwidth(rVal,cex=legValuesCex))][1]
  if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- paste(">=",breakval)}}
  legend_xsize <- max(rLeg[1]*2 + strwidth(longVal,cex=legValuesCex),strwidth(legTitle,cex = legTitleCex)-delta1)

  legend_ysize <-(rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4])*2 + 3*delta2/2 + strheight(legTitle,cex = legTitleCex)
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

mycol <- col1

if(!is.null(breakval)){

  symbols(x = xref + rLeg[1] ,y=yref + delta1 - height/2 + height,circles=height/3,add=TRUE,bg=col1,inches=FALSE)
  symbols(x = xref + rLeg[1] ,y=yref + delta1 - height/2,circles=height/3,add=TRUE,bg=col2,inches=FALSE)
  text(xref + rLeg[1] + height/3 + delta2 ,yref + height/2,paste ("<",breakval),adj=c(0,0.5),cex=legValuesCex)
  text(xref + rLeg[1] + height/3 + delta2,yref + height + height/2,paste (">=",breakval),adj=c(0,0.5),cex=legValuesCex)
  yref <- yref + height *2 + delta2
 mycol <- "white"
}


if (type=="a"){
# cercles (V1)
for(i in 1:4){
symbols(x = xref + rLeg[1] ,y=yref + rLeg[i],circles=rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
}

for(i in 1:4){
  segments(xref+rLeg[1],yref+rLeg[i]*2,xref+rLeg[1]*2+delta2,yref +rLeg[i]*2)
  text(xref+rLeg[1]*2+delta1 ,y= yref+rLeg[i]*2,rVal[i],adj=c(0,0.5),cex=legValuesCex)
}


text(x=xref ,y=yref + delta2 + rLeg[1]*2 + delta2,legTitle,adj=c(0,0),cex=legTitleCex)

}
if (type=="b"){

# cercles (V2)
jump <- 0
for(i in 4:1){
  symbols(x = xref + rLeg[1],y=yref + rLeg[i] + jump,circles=rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
  jump <- jump + rLeg[i]*2 + delta2/2
}

jump <- 0
for(i in 4:1){
  text(xref+rLeg[1]+ rLeg[i] + delta2 ,y= yref+rLeg[i] + jump,rVal[i],adj=c(0,0.5),cex=legValuesCex)
  jump <- jump + rLeg[i]*2 + delta2/2
}
text(x=xref ,y=yref + (rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4])*2 + 3*delta2/2 + delta1,legTitle,adj=c(0,0),cex=legTitleCex)
}

}
}


# #######################################
# LEGENDES POUR CARRES PROPORTIONNELS
# #######################################


#' @title LegendSquaresSymbols
#' @description legend for Prop Symbols layers (squares)
#' @name LegendSquaresSymbols
#' @details plot a legend
#' @param pos bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle
#' @param legTitle Title of the legend
#' @param legTitleCex legTitleCex
#' @param legValuesCex legValuesCex
#' @param varvect Values
#' @param sizevect Sizes of the squares (r)
#' @param breakval breakval
#' @param round round
#' @param col1 color defalul = "red"
#' @param col2 color default = "blue"
#' @param frame if TRUE, a frame is drawn
#' @param type Type of display of the legend. "a" or "b"
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
#' LegendSquaresSymbols(pos = "bottomright", legTitle = "Title of\nthe legend ",
#'                      legTitleCex = 0.8, legValuesCex = 0.6,
#'                      varvect = nuts1.df$pop2008,
#'                      sizevect = sqrt((abs(nuts1.df$pop2008) * 5000)),
#'                      breakval=10, col1="red", col2="blue", frame=TRUE, round=0, type ="a")
#'
#' LegendSquaresSymbols(pos = "topright", legTitle = "Title",
#'                      legTitleCex = 1.2, legValuesCex = 0.8,
#'                      varvect = nuts1.df$pop2008,
#'                      sizevect = sqrt((abs(nuts1.df$pop2008) * 50000)),
#'                      breakval=10, col1="red", col2="blue", frame=TRUE, round=0, type ="b")
#'
#' LegendSquaresSymbols(pos = "topleft", legTitle = "Title\nof\nthe legend",
#'                      legTitleCex = 0.8, legValuesCex = 0.6,
#'                      varvect = nuts1.df$pop2008,
#'                      sizevect = sqrt((abs(nuts1.df$pop2008) * 500)),
#'                      breakval=NULL, col1="red", col2="blue", frame=TRUE, round=0, type ="b")
#' @return plot



LegendSquaresSymbols<- function(pos = "topleft", legTitle = "Title of the legend", legTitleCex = 0.8,
                             legValuesCex = 0.6, varvect, sizevect, breakval, col1="red", col2="blue", frame=FALSE, round=0, type ="a"){


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
  paramsize1 <- 25
  paramsize2 <- 40
  width <- (x2 - x1) / paramsize1
  height <- width /1.5
  delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
  delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets


  rValmax <- max(varvect,na.rm = TRUE)
  rValmin <- min(varvect,na.rm = TRUE)
  rValextent <- rValmax - rValmin
  rLegmax <- max(sizevect,na.rm = TRUE)
  rLegmin <- min(sizevect,na.rm = TRUE)
  rLegextent <- rLegmax - rLegmin

  rVal <- c(rValmax,rValmax - rValextent/3 , rValmax - 2*(rValextent/3),rValmin)
  rLeg <- c(rLegmax,rLegmax - rLegextent/3 , rLegmax - 2*(rLegextent/3),rLegmin)
  rVal <- round(rVal,round)





  # xsize & ysize

  if (type=="a"){
    longVal <- rVal[strwidth(rVal,cex=legValuesCex)==max(strwidth(rVal,cex=legValuesCex))][1]
    if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- paste(">=",breakval)}}
    legend_xsize <- max(rLeg[1] + strwidth(longVal,cex=legValuesCex),strwidth(legTitle,cex = legTitleCex)-delta1)

    legend_ysize <-rLeg[1] + strheight(legTitle,cex = legTitleCex)
    if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2+ delta2}
  }

  if (type=="b"){
    longVal <- rVal[strwidth(rVal,cex=legValuesCex)==max(strwidth(rVal,cex=legValuesCex))][1]
    if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- paste(">=",breakval)}}
    legend_xsize <- max(rLeg[1] + strwidth(longVal,cex=legValuesCex)+ delta2,strwidth(legTitle,cex = legTitleCex))-delta1

    legend_ysize <-rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + strheight(legTitle,cex = legTitleCex)
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

    mycol <- col1

  if(!is.null(breakval)){


    if (type=="a"){
      symbols(x = xref+height/3,y=yref + delta1 - height/2 + height,squares=height/1.5,add=TRUE,bg=col1,inches=FALSE)
      symbols(x = xref+height/3 ,y=yref + delta1 - height/2,squares=height/1.5,add=TRUE,bg=col2,inches=FALSE)
    text(xref + height/1.5 + delta2,yref + height/2,paste ("<",breakval),adj=c(0,0.5),cex=legValuesCex)
    text(xref + height/1.5 + delta2 , yref + height + height/2,paste (">=",breakval),adj=c(0,0.5),cex=legValuesCex)
    yref <- yref + height *2 + delta2
    }
    if (type == "b"){
      symbols(x = xref + rLeg[1]/2,y=yref + delta1 - height/2 + height,squares=height/1.5,add=TRUE,bg=col1,inches=FALSE)
      symbols(x = xref + rLeg[1]/2 ,y=yref + delta1 - height/2,squares=height/1.5,add=TRUE,bg=col2,inches=FALSE)
      text(xref + rLeg[1]/2 + height/3 + delta2,yref + height/2,paste ("<",breakval),adj=c(0,0.5),cex=legValuesCex)
      text(xref + rLeg[1]/2 + height/3 + delta2 , yref + height + height/2,paste (">=",breakval),adj=c(0,0.5),cex=legValuesCex)
      yref <- yref + height *2 + delta2
    }

    mycol <- "white"
  }


  if (type=="a"){
    # squares (V1)
    for(i in 1:4){
      symbols(x = xref + rLeg[i]/2 + (rLeg[1]-rLeg[i]),y=yref + rLeg[i]/2, squares = rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
    }

    for(i in 1:4){
      segments(xref+rLeg[1],yref+rLeg[i],xref+rLeg[1]+delta2,yref +rLeg[i])
      text(xref+rLeg[1] + delta1 ,y= yref+rLeg[i],rVal[i],adj=c(0,0.5),cex=legValuesCex)
    }


    text(x=xref ,y=yref + rLeg[1] + delta1 ,legTitle,adj=c(0,0),cex=legTitleCex)

  }
  if (type=="b"){

    # cercles (V2)
    jump <- rLeg[4]/2
    for(i in 4:1){
      symbols(x = xref + rLeg[i]/2 + (rLeg[1]-rLeg[i])/2 ,y=yref + jump,squares=rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
      text(xref + rLeg[i] + (rLeg[1]-rLeg[i])/2 + delta2 ,y= yref + jump,rVal[i],adj=c(0,0.5),cex=legValuesCex)
      if (i>1){jump <- rLeg[i]/2 +  rLeg[i-1]/2 + jump  + delta2}
    }
    text(x=xref ,y=yref + rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + delta1 ,legTitle,adj=c(0,0),cex=legTitleCex)
  }
  }
}



# #######################################
# LEGENDES POUR BARRES
# #######################################


#' @title LegendHeightSymbols
#' @description legend for Prop PropSymbols layers (height)
#' @name LegendHeightSymbols
#' @details plot a legend
#' @param pos bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle
#' @param legTitle Title of the legend
#' @param legTitleCex legTitleCex
#' @param legValuesCex legValuesCex
#' @param varvect Values
#' @param sizevect Height of the symbols
#' @param breakval breakval
#' @param round round
#' @param col1 color defalul = "red"
#' @param col2 color default = "blue"
#' @param frame if TRUE, a frame is drawn
#' @param type Type of display of the legend. "a" or "b"
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
#' LegendHeightSymbols(pos = "topright", legTitle = "Title of the legend",
#'                     legTitleCex = 0.8, legValuesCex = 0.6,
#'                     varvect = nuts1.df$pop2008,
#'                     sizevect = sqrt((abs(nuts1.df$pop2008) * 1000000) / pi),
#'                     breakval=1000, col1="red", col2="blue", frame=TRUE, round=0, type ="a")
#'
#' LegendHeightSymbols(pos = "left", legTitle = "Title of the legend",
#'                     legTitleCex = 0.8, legValuesCex = 0.6,
#'                     varvect = nuts1.df$pop2008,
#'                     sizevect = sqrt((abs(nuts1.df$pop2008) * 600) / pi),
#'                     breakval=NULL, col1="yellow", col2="blue", frame=FALSE, round=0, type ="b")
#' @return plot

LegendHeightSymbols<- function(pos = "topleft", legTitle = "Title of the legend", legTitleCex = 0.8,
                                legValuesCex = 0.6, varvect, sizevect, breakval, col1="red", col2="blue", frame=FALSE, round=0, type ="a"){


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
    paramsize1 <- 25
    paramsize2 <- 40
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets


    rValmax <- max(varvect,na.rm = TRUE)
    rValmin <- min(varvect,na.rm = TRUE)
    rValextent <- rValmax - rValmin
    rLegmax <- max(sizevect,na.rm = TRUE)
    rLegmin <- min(sizevect,na.rm = TRUE)
    rLegextent <- rLegmax - rLegmin

    rVal <- c(rValmax,rValmax - rValextent/3 , rValmax - 2*(rValextent/3),rValmin)
    rLeg <- c(rLegmax,rLegmax - rLegextent/3 , rLegmax - 2*(rLegextent/3),rLegmin)
    rVal <- round(rVal,round)


    # xsize & ysize

    if (type=="a"){
      longVal <- rVal[strwidth(rVal,cex=legValuesCex)==max(strwidth(rVal,cex=legValuesCex))][1]
      if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- paste(">=",breakval)}}
      legend_xsize <- max(height/1.5 + strwidth(longVal,cex=legValuesCex),strwidth(legTitle,cex = legTitleCex)-delta1)

      legend_ysize <-rLeg[1] + strheight(legTitle,cex = legTitleCex)
      if(!is.null(breakval)){legend_ysize <- legend_ysize + height*2+ delta2}
    }

    if (type=="b"){
      longVal <- rVal[strwidth(rVal,cex=legValuesCex)==max(strwidth(rVal,cex=legValuesCex))][1]
      if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- paste(">=",breakval)}}
      legend_xsize <- max(height/1.5 + strwidth(longVal,cex=legValuesCex)-delta2,strwidth(legTitle,cex = legTitleCex)-delta1)

      legend_ysize <-rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + strheight(legTitle,cex = legTitleCex)
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

    mycol <- col1

    if(!is.null(breakval)){

        symbols(x = xref + delta1 - height/1.5  ,y=yref + delta1 - height/2 + height,squares=height/1.5,add=TRUE,bg=col1,inches=FALSE)
        symbols(x = xref + delta1 - height/1.5,y=yref + delta1 - height/2,squares=height/1.5,add=TRUE,bg=col2,inches=FALSE)
        text(xref + height/1.5 + delta2,yref + height/2,paste ("<",breakval),adj=c(0,0.5),cex=legValuesCex)
        text(xref + height/1.5 + delta2 , yref + height + height/2,paste (">=",breakval),adj=c(0,0.5),cex=legValuesCex)
        yref <- yref + height *2 + delta2

      mycol <- "white"
    }


    if (type=="a"){
      # (V1)


      for(i in 1:4){

        rect(xref, yref, xref+height/1.5, yref+rLeg[i] ,col=mycol)
      }

      for(i in 1:4){
        segments(xref + height/1.5 ,yref+rLeg[i],xref+height/1.5 + delta2,yref +rLeg[i])
        text(xref+  height/1.5 + delta1 ,y= yref+rLeg[i],rVal[i],adj=c(0,0.5),cex=legValuesCex)
      }


      text(x=xref ,y=yref + rLeg[1] + delta1 ,legTitle,adj=c(0,0),cex=legTitleCex)

    }
    if (type=="b"){

      #  (V2)
      jump <- 0
      for(i in 4:1){
        rect(xref, yref+ jump, xref+height/1.5, yref+rLeg[i] + jump ,col=mycol)
        #symbols(x = xref + rLeg[i]/2 + (rLeg[1]-rLeg[i])/2 ,y=yref + jump,squares=rLeg[i],add=TRUE,bg=mycol,inches=FALSE)
        text(xref + height/1.5 + delta2 ,y= yref + jump + rLeg[i]/2,rVal[i],adj=c(0,0.5),cex=legValuesCex)
        if (i>1){jump <- rLeg[i] + delta2 + jump}
      }
      text(x=xref ,y=yref + rLeg[1]+ rLeg[2]+rLeg[3]+rLeg[4] + 3*delta2 + delta1 ,legTitle,adj=c(0,0),cex=legTitleCex)
    }
  }
}

# #######################################
# CHINESE
# #######################################



#' @title LegendChineseSymbols
#' @description legend for Chinese Hats layers
#' @name LegendChineseSymbols
#' @details plot a legend
#' @param pos bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle
#' @param legTitle Title of the legend
#' @param legTitleCex legTitleCex
#' @param legValuesCex legValuesCex
#' @param var1label label 1
#' @param var2label label 1
#' @param vect1 vect1
#' @param vect2 vect2
#' @param sizevect1 size 1
#' @param sizevect2 size2
#' @param round round
#' @param col1 color defalul = "red"
#' @param col2 color default = "blue"
#' @param frame if TRUE, a frame is drawn
#' @param type Type of display of the legend. "a" or "b"
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
#' vect1 <- round((nuts0.df$pop2008 / sum(nuts0.df$pop2008))*100,2)
#' vect2 <- round((nuts0.df$gdppps2008 / sum(nuts0.df$gdppps2008))*100,2)
#' sizevect1 <- sqrt(vect1)/2*100000
#' sizevect2 <- sqrt(vect2)/2*100000
#' LegendChineseSymbols(pos = "topright", var1label = "population totale (habs)",
#'                     var2label = "pib (euros)", legTitle="PIB par habitant",
#'                       legTitleCex = 0.8, legValuesCex = 0.6,
#'                       vect1 = vect1, vect2 = vect2, sizevect1 = sizevect1, sizevect2 = sizevect2,
#'                       col1="green", col2="yellow", frame=TRUE, round=2, type="b")
#'
#' LegendChineseSymbols(pos = "bottomleft", var1label = "population totale (habs)",
#'                       var2label = "pib (euros)", legTitle="PIB par habitant",
#'                       legTitleCex = 0.8, legValuesCex = 0.6,
#'                       vect1 = vect1, vect2 = vect2, sizevect1 = sizevect1, sizevect2 = sizevect2,
#'                       col1="green", col2="yellow", frame=TRUE, round=2, type="a")
#' @return plot


LegendChineseSymbols<- function(pos = "topleft", legTitle, var1label,var2label, legTitleCex = 0.8,
                                legValuesCex = 0.6, vect1, vect2, sizevect1, sizevect2,col1="red", col2="blue", frame=FALSE, round=0, type="a"){

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
    paramsize1 <- 25
    paramsize2 <- 40
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets


    # TOP
    rValmax1 <- round(max(vect1,na.rm = TRUE),round)
    rValmin1 <- round(min(vect1,na.rm = TRUE),round)
    rValextent1 <- rValmax1 - rValmin1
    rLegmax1 <- max(sizevect1,na.rm = TRUE)
    rLegmin1 <- min(sizevect1,na.rm = TRUE)
    rLegextent1 <- rLegmax1 - rLegmin1
    rVal1 <- c(rValmax1,rValmax1 - rValextent1/3 , rValmax1 - 2*(rValextent1/3),rValmin1)
    rLeg1 <- c(rLegmax1,rLegmax1 - rLegextent1/3 , rLegmax1 - 2*(rLegextent1/3),rLegmin1)
    rVal1 <- round(rVal1,round)

   # BOTTOM
   rValmax2 <- round(max(vect2,na.rm = TRUE),round)
   rValmin2 <- round(min(vect2,na.rm = TRUE),round)
   rValextent2 <- rValmax2 - rValmin2
   rLegmax2 <- max(sizevect2,na.rm = TRUE)
   rLegmin2 <- min(sizevect2,na.rm = TRUE)
   rLegextent2 <- rLegmax2 - rLegmin2
   rVal2 <- c(rValmax2,rValmax2 - rValextent2/3 , rValmax2 - 2*(rValextent2/3),rValmin2)
   rLeg2 <- c(rLegmax2,rLegmax2 - rLegextent2/3 , rLegmax2 - 2*(rLegextent2/3),rLegmin2)
   rVal2 <- round(rVal2,round)


    # xsize & ysize
   xmax <- max(rLegmax2, rLegmax1)




   if (type=="a"){
     legend_ysize <- strheight(legTitle,cex=legTitleCex) + delta2 + rLegmax2/2 + rLegmax1/2 + delta1 + height *2 + delta2 + rLegmin2/2 + rLegmin1/2 + (rLegmin2 + rLegmax2)/4 + + (rLegmin1 + rLegmax1)/4 + 3*delta2
   }
   if (type =="b"){
     legend_ysize <- strheight(legTitle,cex=legTitleCex) + delta2 + rLegmax2/2 + rLegmax1/2 + delta1 + height *2 + delta2

   }




   longVal <- rVal1[strwidth(rVal1,cex=legValuesCex)==max(strwidth(rVal1,cex=legValuesCex))][1]
   longVal2 <- rVal2[strwidth(rVal2,cex=legValuesCex)==max(strwidth(rVal2,cex=legValuesCex))][1]
   if (strwidth(longVal2,cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- longVal2}
   legend_xsize <- strwidth(longVal,cex=legValuesCex) + max(rLegmax2,rLegmax1)
   tmp <- strwidth(legTitle,cex=legTitleCex)
   if (tmp > legend_xsize) {legend_xsize <- tmp}
   tmp <- max(strwidth(var1label,cex=legValuesCex),strwidth(var2label,cex=legValuesCex)) + height + delta2
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


symbols(x = xref + max(rLeg1[1],rLeg2[1])/2,y=yref + delta1 - height/2 + height,squares=height/1.5,add=TRUE,bg=col1,inches=FALSE)
symbols(x = xref + max(rLeg1[1],rLeg2[1])/2 ,y=yref + delta1 - height/2,squares=height/1.5,add=TRUE,bg=col2,inches=FALSE)
text(xref + max(rLeg1[1],rLeg2[1])/2 + height/3 + delta2,yref + height/2,var2label,adj=c(0,0.5),cex=legValuesCex)
text(xref + max(rLeg1[1],rLeg2[1])/2 + height/3 + delta2 , yref + height + height/2,var1label,adj=c(0,0.5),cex=legValuesCex)
yref <- yref + height *2 + delta2



  if (type=="a"){


    text(x=xref ,y=yref + (rLegmax1 + rLegmax2 + rLegmin1 + rLegmin2 + (rLegmax1-rLegmin1)/2 +  (rLegmax2-rLegmin2)/2)/2
         + 4*delta1,adj=c(0,0),legTitle,cex=legTitleCex)


  mysize <- rLegmax2 ;  yadd <-  mysize/2
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref + yadd), col = col2)
  segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
  text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,rValmax2,cex=legValuesCex,adj=c(0,0.5))

  mysize <- (rLegmax2 - rLegmin2)/2 ; yadd <-  mysize/2 + yadd + delta2
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref+yadd), col = col2)
  segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
  text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,round((rValmax2 + rValmin2)/2,round),cex=legValuesCex,adj=c(0,0.5))

  mysize <- rLegmin2 ; yadd <-  mysize/2 + yadd + delta2
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref+ yadd,yref-mysize/2+yadd,yref+yadd), col = col2)
  segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
  text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,rValmax2,cex=legValuesCex,adj=c(0,0.5))


    mysize <- rLegmin1 ; yadd <-  yadd + delta2
    polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + delta2 + yadd,yref+mysize/2 + delta2 + yadd,yref + delta2 + yadd), col = col1)
    segments(xref + xmax/2,yref+mysize/2 + delta2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + delta2 + yadd)
    text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + delta2 + yadd,rValmin1,cex=legValuesCex,adj=c(0,0.5))

    yadd <-  mysize/2 + yadd + delta2 ; mysize <- (rLegmax1 - rLegmin1)/2
    polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + delta2 + yadd,yref+mysize/2 + delta2  + yadd,yref + delta2 + yadd), col = col1, )
    segments(xref + xmax/2,yref+mysize/2 + delta2  + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + delta2  + yadd)
    text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + delta2  + yadd,round((rValmin1 +rValmax1)/2,round) ,cex=legValuesCex,adj=c(0,0.5))

    yadd <-  mysize/2 + yadd  + delta2 ; mysize <- rLegmax1
    polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + delta2 + yadd,yref+mysize/2 + delta2 + yadd,yref + delta2 + yadd), col = col1)
    segments(xref + xmax/2,yref+mysize/2 + delta2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + delta2 + yadd)
    text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + delta2 + yadd,rValmax1,cex=legValuesCex,adj=c(0,0.5))



  }

if (type=="b"){

  text(x=xref ,y=yref + rLegmax2/2 + rLegmax1/2 + delta1 + delta1,adj=c(0,0),legTitle,cex=legTitleCex)

  mysize <- rLegmax2 ;  yadd <-  mysize/2
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref + yadd), col = col2)
  segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
  text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,rValmax2,cex=legValuesCex,adj=c(0,0.5))

  mysize <- (rLegmax2 - rLegmin2)/2 ;
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + yadd,yref-mysize/2 + yadd,yref+yadd), col = col2)
  segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
  text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,round((rValmax2 + rValmin2)/2,round),cex=legValuesCex,adj=c(0,0.5))

  mysize <- rLegmin2 ;
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref+ yadd,yref-mysize/2+yadd,yref+yadd), col = col2)
  segments(xref + xmax/2,yref-mysize/2 + yadd, xref + xmax/2 + delta1 + xmax/2 ,yref-mysize/2 + yadd)
  text(xref + xmax/2 + delta1 + xmax/2 + delta2 ,yref-mysize/2 + yadd,rValmax2,cex=legValuesCex,adj=c(0,0.5))

  mysize <- rLegmax1
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + rLegmax2/2 + delta2,yref+mysize/2 + rLegmax2/2 + delta2,yref + rLegmax2/2 + delta2), col = col1)
  segments(xref + xmax/2,yref+mysize/2 + rLegmax2/2 + delta2, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2)
  text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2,rValmax1,cex=legValuesCex,adj=c(0,0.5))

  mysize <- (rLegmax1 - rLegmin1)/2
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + rLegmax2/2 + delta2,yref+mysize/2 + rLegmax2/2 + delta2,yref + rLegmax2/2 + delta2), col = col1, )
  segments(xref + xmax/2,yref+mysize/2 + rLegmax2/2 + delta2, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2)
  text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2,round((rValmin1 +rValmax1)/2,round) ,cex=legValuesCex,adj=c(0,0.5))

  mysize <- rLegmin1
  polygon(c(xref-mysize/2 + xmax/2,xref + xmax/2,xref+mysize/2 + xmax/2), c(yref + rLegmax2/2 + delta2,yref+mysize/2 + rLegmax2/2 + delta2,yref + rLegmax2/2 + delta2), col = col1)
  segments(xref + xmax/2,yref+mysize/2 + rLegmax2/2 + delta2, xref + xmax/2 + delta1 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2)
  text(xref + xmax/2 + delta1 + delta2 + xmax/2 ,yref+mysize/2 + rLegmax2/2 + delta2,rValmin1,cex=legValuesCex,adj=c(0,0.5))

}





  }
}


# #######################################
# LEGENDES POUR LIGNES PROPORTIONNELLES
# #######################################

#' @title LegendPropLines
#' @description legend for Prop Lines layers
#' @name LegendPropLines
#' @details plot a legend
#' @param pos bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle
#' @param legTitle Title of the legend
#' @param legTitleCex legTitleCex
#' @param legValuesCex legValuesCex
#' @param varvect Values
#' @param sizevect Sizes of the squares (r)
#' @param round round
#' @param col color defalul = "red"
#' @param frame if TRUE, a frame is drawn
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], border = "black")
#'
#' LegendPropLines(pos = "topleft", legTitle = "Title\nxkkxkx",
#'                 legTitleCex = 0.8, legValuesCex = 0.6,
#'                 varvect = nuts1.df$pop2008,
#'                 sizevect = nuts1.df$pop2008/1000000,
#'                 col="red", frame=TRUE, round=2)
#' @return plot



LegendPropLines<- function(pos = "topleft", legTitle = "Title of the legend", legTitleCex = 0.8,
                               legValuesCex = 0.6, varvect, sizevect, col="red", frame=FALSE, round=0){


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
    paramsize1 <- 25
    paramsize2 <- 40
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets


    rValmax <- max(varvect,na.rm = TRUE)
    rValmin <- min(varvect,na.rm = TRUE)
    rValextent <- rValmax - rValmin
    rLegmax <- max(sizevect,na.rm = TRUE)
    rLegmin <- min(sizevect,na.rm = TRUE)
    rLegextent <- rLegmax - rLegmin

    rVal <- c(rValmax,rValmax - rValextent/3 , rValmax - 2*(rValextent/3),rValmin)
    rLeg <- c(rLegmax,rLegmax - rLegextent/3 , rLegmax - 2*(rLegextent/3),rLegmin)
    rVal <- round(rVal,round)

    # xsize & ysize

      longVal <- rVal[strwidth(rVal,cex=legValuesCex)==max(strwidth(rVal,cex=legValuesCex))][1]
      #if(!is.null(breakval)){if (strwidth(paste(">=",breakval),cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- paste(">=",breakval)}}
      legend_xsize <- max(width+ strwidth(longVal,cex=legValuesCex)-delta2,strwidth(legTitle,cex = legTitleCex)-delta1)

      legend_ysize <-8*delta1 + strheight(legTitle,cex = legTitleCex)

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
        text(xref + width + delta2 ,y= yref + jump,rVal[i],adj=c(0,0.5),cex=legValuesCex)
        jump <- jump + 2*delta1 # ICI AMELIORER
      }
      text(x=xref ,y=yref + 9*delta1,legTitle,adj=c(0,0),cex=legTitleCex)
  }
}

# #######################################
# LEGENDES POUR LIGNES PAR CLASSES DE TAILLES
# #######################################


#' @title LegendSizeLines
#' @description legend for classes of  Lines sizes layers
#' @name LegendSizeLines
#' @details plot a legend
#' @param pos bottomleft, topleft, topright, bottomright, left, right, top, bottom, middle
#' @param legTitle Title of the legend
#' @param legTitleCex legTitleCex
#' @param legValuesCex legTitleCex
#' @param distr vector of classes
#' @param thickness vector of thickness
#' @param round round
#' @param col color defalul = "red"
#' @param frame if TRUE, a frame is drawn
#' @param nodata if TRUE, a box "no data" is drawn
#' @param nodatalabel label for no data.
#' @export
#' @examples
#' data("nuts2006")
#' plot(nuts0.spdf)
#' LegendSizeLines(pos = "bottomleft", legTitle = "Title of the legend", legTitleCex = 0.8,
#'                 legValuesCex = 0.6, distr = c(1,2,3,4,10.2,15.2),
#'                 thickness = c(0.2,2,5,10,27),
#'                 col ="blue", round =2,
#'                 nodata = TRUE,
#'                 nodatalabel = "No data available", frame = TRUE)
#' @return plot

LegendSizeLines <- function(pos = "topleft", legTitle = "Title of the legend", legTitleCex = 0.8,
                        legValuesCex = 0.6, distr,thickness, col, round =2, nodata = TRUE, nodatalabel = "No data", frame=FALSE){

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
    paramsize1 <- 25
    paramsize2 <- 40
    width <- (x2 - x1) / paramsize1
    height <- width /1.5
    delta1 <- min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2) # Gros eccart entre les objets
    delta2 <- (min((y2 - y1) / paramsize2, (x2 - x1) / paramsize2))/2 # Petit eccart entre les objets
    #rect(x1, y1, x2, y2, border = "black")


    # Taille du bloc de legende
    distr <- as.numeric(round(distr, round))
    longVal <- distr[strwidth(distr,cex=legValuesCex)==max(strwidth(distr,cex=legValuesCex))][1]
    if (nodata == TRUE){if (strwidth(nodatalabel,cex=legValuesCex)>strwidth(longVal,cex=legValuesCex)){longVal <- nodatalabel}}
    legend_xsize <- max(width + strwidth(longVal,cex=legValuesCex),strwidth(legTitle,cex = legTitleCex) - delta2) - delta2
    legend_ysize <- length(distr)*height + (length(distr)-2)*delta2 +  strheight(legTitle,cex = legTitleCex)

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
      rect(xref,yref ,xref + width,yref + height,col="white",border="black",lwd=0.4)
      text(xref + width + delta2 ,yref + height/2 ,nodatalabel,adj=c(0,0.5),cex=legValuesCex)
      yref <- yref + height + delta2
    }

      for (i in 0:(length(distr)-2)){
        j <- i+1
        segments(xref, yref + height/2+ i*height+i*delta2 + (height+delta2)/2, xref + width, yref + i*height+i*delta2 + height/2+ (height+delta2)/2, lwd=thickness[j], col=col, lend=1)
        #rect(xref,yref + i*height + i*delta2,xref + width,yref + height + i*height + i*delta2,col=cols[i+1],border="black",lwd=0.4)
        text(xref + width + delta2 ,y= yref + height/2 + i * height + i*delta2,distr[j],adj=c(0,0.5),cex=legValuesCex)
        }
        text(xref + width + delta2 ,y= yref + height/2 + (i+1)* height + (i+1)*delta2,distr[j+1],adj=c(0,0.5),cex=legValuesCex)


    # Affichage du titre
    text(x=xref,y=yref + length(distr)*height + length(distr)*delta2,legTitle,adj=c(0,0),cex=legTitleCex)

  }

}

