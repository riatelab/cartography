################################################################################
### General utils

#' @name convertToSf
#' @title convertToSf
#' @description convert spdf & df to sf
#' @param spdf spdf
#' @param df df
#' @param spdfid spdfid 
#' @param dfid dfid
#' @return an sf object
#' @noRd
convertToSf <- function(spdf, df, spdfid, dfid){
  if (!missing(df)){
    # missing IDs
    if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
    if (is.null(dfid)){dfid <- names(df)[1]}
    # Join (only on df data), work with tibbls
    spdf@data <- data.frame(spdf@data[,spdfid], 
                            data.frame(df[match(spdf@data[,spdfid], 
                                                df[[dfid]]),]))
    spdf <- spdf[!is.na(spdf@data[,dfid]),]
  }
  # convert
  x <- sf::st_as_sf(spdf)
  return(x)
}


################################################################################
### choro utils

#' @name choro
#' @title choro
#' @description add color gradients to spdf according to data classes
#' @param var vector of values used
#' @param distr vector of classes
#' @param col vector of colors
#' @param nclass number of classes targeted (if null, the Huntsberger method is used)
#' @param method discretization method ("sd", "equal", "quantile", "fisher-jenks","q6","geom")
#' @return List: a vector of colors, colors and distr
#' @noRd
choro <- function(var, distr = NULL, col = NULL, 
                  nclass = NULL, method = "quantile")
{
  # Discretization
  if (is.null(distr)){
    distr <- getBreaks(v = var, nclass = nclass, method = method)
    
    # Colors
    if(is.null(col)){
      col <- carto.pal(pal1 = "blue.pal",n1 = (length(distr) - 1))
    }
    
    colMap <- col[findInterval(var, distr, all.inside = TRUE)]
  }else{
    inter <- findInterval(var, distr, all.inside = FALSE, 
                          rightmost.closed	= TRUE)
    inter[inter == 0] <- length(distr)
    if(is.null(col)){
      col <- carto.pal(pal1 = "blue.pal",n1 = (length(distr) - 1))
    }
    colMap <- col[inter]
  }

  return(list(colMap = colMap, distr = distr, col = col))
}



################################################################################
### typo utils

#' @name checkCol
#' @title checkCol
#' @description check if col length matches modalities length, if no color is 
#' provided add default colors
#' @param col vector of colors
#' @param mod vector of modalities
#' @return  a vector of colors.
#' @noRd
checkCol <- function(col, mod){
  if (is.null(col)){
    lm <- length(mod)
    if (lm<=20){
      col <- carto.pal(pal1 = "pastel.pal", n1 = lm)
    }else{
      lc <- carto.pal(pal1 = "pastel.pal", 20)
      col <- sample(x = lc, size = lm , replace = T)
    } 
  }else{
    if (length(col) < length(mod)){
      stop(paste("'col' length (",length(col),
                 ") must fit the number of modalities of the variable (",
                 length(mod),").",sep = ""),
           call. = FALSE)
    }
  }
  return(col)
}

#' @name checkOrder
#' @title checkOrder
#' @description check if col order match legend.values.order
#' @param legend.values.order legend.values.order
#' @param mod vector of modalities
#' @return  a vector of legend.values.order.
#' @noRd
checkOrder <- function(legend.values.order, mod){
  if (!is.null(legend.values.order)){
    m <- match(mod, legend.values.order)
    m <- m[!is.na(m)]
    
    if(length(m) != length(mod) | length(mod) != length(legend.values.order)){
      stop(paste("'legend.values.order' modalities must fit the modalities of the variable (",
                 paste(mod, collapse=","),").",sep = ""),
           call. = FALSE)
    }
  }else{
    legend.values.order <- mod
  }
  return(legend.values.order)
}



################################################################################
### prop symbols utils

#' @name checkMergeOrder
#' @title checkMergeOrder
#' @description clean, sorted sf object with centroid coordinates from an 
#' sf object 
#' @param x x 
#' @param var var 
#' @return an sorted and cleaned sf object with centroid coordinates.
#' @noRd
checkMergeOrder <- function(x = x, var = var){
  # get centroid coords
  x <- cbind(sf::st_coordinates(
    sf::st_centroid(x = x, of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))), x)
  # remove NAs and 0 values
  x <- x[!is.na(x = x[[var]]),]
  x <- x[x[[var]]!=0, ]
  # turn to positive values
  x[[var]] <- abs(x[[var]])
  # Order the dots
  x <- x[order(abs(x[[var]]), decreasing = TRUE),]
  return(x)
}

#' @name sizer
#' @title sizer
#' @description get a vector of radii
#' @param dots dots 
#' @param inches inches
#' @param var var
#' @param fixmax fixmax
#' @param symbols symbols 
#' @return a vector of radii
#' @noRd
sizer <- function(dots, inches, var, fixmax, symbols){
  switch(symbols, 
         circle = {
           smax <- inches * inches * pi
           size <- sqrt((abs(dots[[var]]) * smax  / fixmax) / pi)
         }, 
         square = {
           smax <- inches * inches
           size <- sqrt(abs(dots[[var]]) * smax   / fixmax)
         }, 
         bar = {
           smax <- inches
           size <- abs(dots[[var]]) * smax  / fixmax
         })
  return(size)
}



################################################################################
### legend utils
legpos <- function(pos, x1, x2, y1, y2, delta1, delta2, 
                   legend_xsize, legend_ysize){
  # Position
  if(length(pos) == 2){
    return(list(xref = pos[1], yref = pos[2]))
  }
  if (pos == "bottomleft") {
    xref <- x1 + delta1
    yref <- y1 + delta1
  }
  if (pos == "topleft") {
    xref <- x1 + delta1
    yref <- y2 - 2 * delta1 - legend_ysize
  }
  if (pos == "topright") {
    xref <- x2 - 2 * delta1 - legend_xsize
    yref <- y2 -2 * delta1 - legend_ysize
  }
  if (pos == "bottomright") {
    xref <- x2 - 2 * delta1 - legend_xsize
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
  if (pos == "center") {
    xref <- (x1+x2)/2 - legend_xsize/2
    yref <- (y1+y2)/2-legend_ysize/2 - delta2
  }
  return(list(xref = xref, yref = yref))
}


################################################################################
### labelLayer utils

# Rcpp stuff
#' @useDynLib cartography, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL


# Label placement
#' @name wordlayout
#' @title wordlayout
#' @description wordlayout
#' @param x long
#' @param y lat
#' @param words labels
#' @param cex cex
#' @param rotate90 rotate
#' @param xlim xlim
#' @param ylim ylim
#' @param tstep tstep
#' @param rstep rstep
#' @param ... other stuf
#' @return coords
#' @noRd
wordlayout <- function(x, y, words, cex=1, rotate90 = FALSE,
                       xlim=c(-Inf,Inf), ylim=c(-Inf,Inf), 
                       tstep=.1, rstep=.1, ...){
  tails <- "g|j|p|q|y"
  n <- length(words)
  sdx <- sd(x,na.rm=TRUE)
  sdy <- sd(y,na.rm=TRUE)
  if(sdx==0)
    sdx <- 1
  if(sdy==0)
    sdy <- 1
  if(length(cex)==1)
    cex <- rep(cex,n)
  if(length(rotate90)==1)
    rotate90 <- rep(rotate90,n)	
  set.seed(999)
  boxes <- list()
  for(i in 1:length(words)){
    rotWord <- rotate90[i]
    r <-0
    theta <- runif(1,0,2*pi)
    x1 <- xo <- x[i]
    y1 <- yo <- y[i]
    wid <- strwidth(words[i],cex=cex[i],...) + 0.4 * 
      strwidth("R", cex=cex[i], ...)
    ht <- strheight(words[i],cex=cex[i],...) + 0.4 * 
      strheight("R", cex=cex[i], ...)

    #mind your ps and qs
    if(grepl(tails,words[i]))
      ht <- ht + ht*.2
    if(rotWord){
      tmp <- ht
      ht <- wid
      wid <- tmp	
    }
    isOverlaped <- TRUE
    while(isOverlaped){
      if(!is_overlap(x1-.5*wid,y1-.5*ht,wid,ht,boxes) &&
         x1-.5*wid>xlim[1] && y1-.5*ht>ylim[1] &&
         x1+.5*wid<xlim[2] && y1+.5*ht<ylim[2]){
        boxes[[length(boxes)+1]] <- c(x1-.5*wid,y1-.5*ht,wid,ht)
        isOverlaped <- FALSE
      }else{
        theta <- theta+tstep
        r <- r + rstep*tstep/(2*pi)
        x1 <- xo+sdx*r*cos(theta)
        y1 <- yo+sdy*r*sin(theta)
      }
    }
  }
  result <- do.call(rbind,boxes)
  colnames(result) <- c("x","y","width","ht")
  rownames(result) <- words
  result
}


# shadow around the labels
#' @name shadowtext
#' @title shadowtext
#' @description shadowtext
#' @param x lon
#' @param y lat
#' @param labels labels
#' @param col col
#' @param bg bg
#' @param theta number of iteration 
#' @param r radius
#' @param ... 
#' @noRd
shadowtext <- function(x, y=NULL, labels, col='white', bg='black', 
                       theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {
  xo <- r*strwidth('A')
  yo <- r*strheight('A')
  for (i in theta) {
    text(x + cos(i)*xo, y + sin(i)*yo, labels, col=bg, ... )
  }
  text(x, y, labels, col=col, ... )
}


# import stuffs
#' @importFrom graphics image legend lines par plot.new
#'             plot.window points polygon rect segments
#'             strheight strwidth symbols text title 
#'             xinch yinch plot
#' @importFrom stats aggregate na.omit quantile runif sd
#' @importFrom rgeos createSPComment
NULL
