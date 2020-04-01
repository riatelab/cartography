#' @title Waffle Layer
#' @name waffleLayer
#' @description Plot a waffle layer.
#' @param x an sf object, a simple feature collection.
#' @param var names of the numeric variable to plot.
#' @param cellvalue value of a single cell. Original values are rounded, using
#' \code{cellrnd} method, to be expressed as multiple of \code{cellvalue}. 
#' @param cellsize size of single cell, in map units.
#' @param cellrnd rounding method, one of "ceiling", "floor", "round". 
#' @param celltxt text that appears under the legend.
#' @param labels names that will appear in the legend.
#' @param ncols number of columns of the waffles
#' @param col a vector of colors.
#' @param border color of the cells borders.
#' @param lwd cells borders width.
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
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"),
#'                quiet = TRUE)
#' # number of employed persons
#' mtq$EMP <- mtq$ACT - mtq$CHOM
#' 
#' plot(st_geometry(mtq),
#'      col = "#f2efe9",
#'      border = "#b38e43",
#'      lwd = 0.5)
#' waffleLayer(
#'   x = mtq,
#'   var = c("EMP", "CHOM"),
#'   cellvalue = 100,
#'   cellsize = 400,
#'   cellrnd = "ceiling",
#'   celltxt = "1 cell represents 100 persons",
#'   labels = c("Employed", "Unemployed"),
#'   ncols = 6,
#'   col = c("tomato1", "lightblue"),
#'   border = "#f2efe9",
#'   legend.pos = "topright",
#'   legend.title.cex = 1,
#'   legend.title.txt = "Active Population",
#'   legend.values.cex = 0.8,
#'   add = TRUE
#' )
#' 
#' layoutLayer(
#'   title = "Structure of the Active Population",
#'   col = "tomato4",
#'   tabtitle = TRUE,
#'   scale = FALSE,
#'   sources =  paste0("cartography ", packageVersion("cartography")),
#'   author = "Sources: Insee and IGN, 2018",
#' )
waffleLayer <- function(x, var, 
                        cellvalue,
                        cellsize, 
                        cellrnd = "ceiling", 
                        celltxt = paste0("1 cell = ", cellvalue),
                        labels, 
                        ncols,
                        col,
                        border = "white", lwd = .2,
                        legend.pos = "bottomleft", 
                        legend.title.txt = "legend title",
                        legend.title.cex = 0.8, 
                        legend.values.cex = 0.6,
                        legend.frame = FALSE,
                        add = TRUE){
  
  if(!cellrnd %in% c("round", "floor", "ceiling")){
    stop('cellrnd should be set to "round", "floor" or "ceiling"', 
         call. = FALSE)
  }
  x[, var] <- switch(
    cellrnd,
    ceiling = ceiling(x[, var, drop = TRUE] / cellvalue),
    round   = round(x[, var, drop = TRUE] / cellvalue, 0),
    floor   = floor(x[, var, drop = TRUE] / cellvalue)
  )
  
  x <- x[rowSums(x[,var, drop = T])>0, ]
  
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
    grid <- sf::st_make_grid( offset = c(xcenter,ycenter), 
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
            col = rev(col), legend.pos = NA)
  
  
  legendWaffle(pos = legend.pos, 
               title.txt = legend.title.txt,
               title.cex = legend.title.cex, 
               values.cex = legend.values.cex,
               categ = labels, 
               col = col, 
               frame = legend.frame, 
               cell.size = cellsize,
               cell.txt = celltxt,
               lwd = .0, border = NA)
  
}





