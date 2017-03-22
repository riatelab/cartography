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
    # Join (only on df data)
    spdf@data <- data.frame(spdf@data[,spdfid], df[match(spdf@data[,spdfid], df[,dfid]),])
    spdf <- spdf[!is.na(spdf@data[,dfid]),]
  }
  # convert
  x <- sf::st_as_sf(spdf)
  return(x)
}



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
    
    colMap <- col[findInterval(var,distr,all.inside=TRUE)]
  }else{
    inter <-findInterval(var,distr,all.inside=FALSE, rightmost.closed	=T)
    inter[inter==0] <- length(distr)
    if(is.null(col)){
      col <- carto.pal(pal1 = "blue.pal",n1 = (length(distr) - 1))
    }
    colMap <- col[inter]
  }

  return(list(colMap = colMap, distr = distr, col = col))
}




### typo utils

#' @name checkCol
#' @title checkCol
#' @description check if col length matches modalities length, if no color is provided add default colors
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



