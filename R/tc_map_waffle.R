#' @title Plot a waffle map
#' @name tc_map_waffle
#' @description Plot a waffle layer.
#' @param x an sf object, a simple feature collection.
#' @param var names of the numeric variable to plot.
#' @param cell_value value of a single cell. Original values are rounded, using
#' \code{cellrnd} method, to be expressed as multiple of \code{cellvalue}. 
#' @param cell_size size of single cell, in map units.
#' @param cell_rnd rounding method, one of "ceiling", "floor", "round". 
#' @param cell_txt text that appears under the legend.
#' @param labels names that will appear in the legend.
#' @param ncols number of columns of the waffles
#' @param pal a vector of colors.
#' @param border color of the cells borders.
#' @param lwd cells borders width.
#' @param leg_pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
#' legend.pos is "n" then the legend is not plotted.
#' @param leg_title title of the legend.
#' @param leg_title_cex size of the legend title.
#' @param leg_val_cex size of the values in the legend.
#' @param leg_frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @export
#' @examples
#' library(sf)
#' mtq <- tc_import_mtq()
#' # number of employed persons
#' mtq$EMP <- mtq$ACT - mtq$CHOM
#' tc_map(mtq,
#'        col = "#f2efe9",
#'        border = "#b38e43",
#'        lwd = 0.5)
#' tc_map_waffle(
#'   x = mtq,
#'   var = c("EMP", "CHOM"),
#'   cell_value = 100,
#'   cell_size = 400,
#'   cell_rnd = "ceiling",
#'   cell_txt = "1 cell represents 100 persons",
#'   labels = c("Employed", "Unemployed"),
#'   ncols = 4,
#'   pal = c("tomato1", "lightblue"),
#'   border = "#f2efe9",
#'   leg_pos = "topright",
#'   leg_title_cex = 1,
#'   leg_title = "Active Population",
#'   leg_val_cex = 0.8,
#'   add = TRUE
#' )
#' tc_layout(
#'   title = "Structure of the Active Population",
#'   scale = FALSE,
#'   credits = "Sources: Insee and IGN, 2018",
#' )
tc_map_waffle <- function(x, var, 
                        cell_value,
                        cell_size, 
                        cell_rnd = "ceiling", 
                        cell_txt = paste0("1 cell = ", cell_value),
                        labels, 
                        ncols,
                        pal,
                        border = "white", lwd = .2,
                        leg_pos = "bottomleft", 
                        leg_title = "legend title",
                        leg_title_cex = 0.8, 
                        leg_val_cex = 0.6,
                        leg_frame = FALSE,
                        add = TRUE){
  
  if(!cell_rnd %in% c("round", "floor", "ceiling")){
    stop('cellrnd should be set to "round", "floor" or "ceiling"', 
         call. = FALSE)
  }
  x[, var] <- switch(
    cell_rnd,
    ceiling = ceiling(x[, var, drop = TRUE] / cell_value),
    round   = round(x[, var, drop = TRUE] / cell_value, 0),
    floor   = floor(x[, var, drop = TRUE] / cell_value)
  )
  
  x <- x[rowSums(x[,var, drop = T])>0, ]
  
  # default labels
  if(missing(labels)){
    labels <- var
  }
  # default cellsize
  if(missing(cell_size)){
    bb <- sf::st_bbox(x)
    cell_size <- round(setdiff(bb[3], bb[1]) / 1500, 0)
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
    
    xcenter <- df[i, ncol(df)-1] - (xx * cell_size / 2) 
    ycenter <- df[i, ncol(df)] - (nh * cell_size / 2) 
    
    # create the waffle
    grid <- sf::st_make_grid( offset = c(xcenter,ycenter), 
                              cellsize = cell_size, 
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
  
  # if(missing(col)){
  #   col <- NULL
  # }
  # 
  # col <- checkCol(col, var)
  
  tc_map_t(gg, var="typo", 
          val_order = rev(var),
          add = add, lwd = lwd, 
          border = border,
          pal = rev(pal), leg_pos = NA)
  
  
  tc_leg_waffle(pos = leg_pos,
               title.txt = leg_title,
               title.cex = leg_title_cex,
               values.cex = leg_val_cex,
               categ = labels,
               col = pal,
               frame = leg_frame,
               cell.size = cell_size,
               cell.txt = cell_txt,
               lwd = .0, border = NA)
  
}





