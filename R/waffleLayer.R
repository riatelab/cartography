#' @title Waffle Layer
#' @name waffleLayer
#' @description Plot a waffle layer.
#' @param x an sf object, a simple feature collection.
#' @param var names of the numeric variable to plot.
#' @param value value of a single cell (variables will be rounded to be 
#' expressed as multiple of value). See Details
#' @param labels names that will appear in the legend.
#' @param ncols number of columns of the waffles
#' @param cellsize size of single cell, in map units.
#' @param col a vector of colors.
#' @param border color of the polygons borders.
#' @param lwd borders width.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @export
#' @examples
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' mtq$EMP <- mtq$ACT - mtq$CHOM
#' mtq$INACT <- mtq$POP - mtq$ACT
#' 
#' par(mar=c(0,0,0,0))
#' 
#' plot(st_geometry(mtq), col="#f2efe9", border="#b38e43", bg = "#aad3df",
#'      lwd = 0.5)
#' waffleLayer(x = mtq, var = rev(c("CHOM","EMP", "INACT")), 
#'             value = 200, cellsize = 300, ncols = 10)
waffleLayer <- function(x, var, value, labels, ncols = 5, cellsize, 
                        col,
                        border = "white", lwd = .2,
                        legend.pos = "bottomleft", 
                        legend.title.txt = var,
                        legend.title.cex = 0.8, 
                        legend.values.cex = 0.6,
                        legend.frame = FALSE,
                        add = FALSE){
  x[,var] <- ceiling(x[,var,drop=TRUE] / value)
  
  # default labels
  if(missing(labels)){
    labels <- var
  }
  # default cellsize
  if(missing(cellsize)){
    bb <- sf::st_bbox(x)
    cellsize <- round(setdiff(bb[3], bb[1]) / 1500, 0)
  }
  if(missing(ncols)){
    ncols <- 5
  }
  
  # get projection  
  mycrs <- sf::st_crs(x)
  
  df <- cbind(
    sf::st_set_geometry(x = x, NULL),
    sf::st_coordinates(sf::st_centroid(sf::st_geometry(x), 
                                       of_largest_polygon = TRUE))[,1:2]
  )
  
  waf_l <- vector("list", nrow(df))
  
  for (i in 1:nrow(df)){
    v <- df[i, var]
    # total number of cells
    nc <- sum(v)
    # Should we add a row, nh = number of rows
    mod <- nc %% ncols
    if(mod > 0){ 
      nh <- ceiling(nc / ncols)
    }else{
      nh <- floor(nc / ncols)
    }
    
    # Center the waffle correctly based on the number of cols and rows
    if(ncols > nc){
      xx <- nc
    }else{
      xx <- ncols
    }
    
    xcenter <- df[i, "X"] - (xx * cellsize / 2) 
    ycenter <- df[i, "Y"] - (nh * cellsize / 2) 
    
    # create the waffle
    grid <- sf::st_make_grid(x = x, offset = c(xcenter,ycenter), 
                             cellsize = cellsize, 
                             n = c(ncols, nh), crs = mycrs)
    # populate the waffle with value
    lx <- character(0)
    for (j in 1:length(var)){
      lx <- c(lx,rep(var[j],v[j]))
    }
    grid <- sf::st_sf(typo = NA, grid)
    grid[1:length(lx),"typo"] <- lx
    waf_l[[i]] <- grid[!is.na(grid$typo),]
  }
  gg <- do.call(rbind, waf_l)
  
  if(missing(col)){
    col <- NULL
  }
  
  col <- checkCol(col, var)
  
  typoLayer(gg, var="typo", 
            legend.values.order = rev(var),
            add = add, lwd = lwd, 
            border = border, 
            col = col, legend.pos = NA)
}





