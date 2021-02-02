#' @title Hatched Layer
#' @name hatchedLayer
#' @description Plot a hatched layer with several different patterns. Suitable 
#' for b/w print maps.
#' @param x an sf object, a simple feature collection. It should be either a 
#' \code{POLYGON} or a \code{MULTIPOLYGON}.
#' @param pattern Desired pattern to use for hatching. Possible values are:
#' \itemize{
#'   \item Dots: \code{"dot", "text"}
#'   \item Lines \code{"diamond","grid","hexagon","horizontal", "vertical",
#'   "zigzag","left2right","right2left","circle"}
#' }
#' @param density of the grid. By default the function uses a grid with a 
#' minimum of 10 cells on the shortest dimension of the bounding box. 
#' Additionally, it is possible to pass a 
#' \code{\link[sf:st_make_grid]{cellsize}} value that would feed the 
#' \code{\link[sf:st_make_grid]{st_make_grid}} underlying function.
#' @param txt for the \code{"text"} pattern, that should be a character.
#' @param ... Additional graphic parameters (see Details).
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @return When passing \code{mode='sfc'} an 'sf' object (either MULTLINESTRING
#'  or MULTIPOINT) is returned.
#' @details Possible values are:
#' \tabular{lccccccc}{
#' \bold{pattern}        \tab \bold{add} \tab \bold{col} \tab \bold{bg} \tab \bold{cex} \tab \bold{pch} \tab \bold{lwd} \tab \bold{lty}\cr
#' \bold{"dot"}          \tab x   \tab x   \tab x  \tab x   \tab x   \tab     \tab    \cr
#' \bold{"text"}         \tab x   \tab x   \tab    \tab x   \tab     \tab     \tab    \cr
#' \bold{Lines patterns} \tab x   \tab x   \tab    \tab     \tab     \tab x   \tab x  
#' }
#' @seealso \link{tc_pattern}
#' @keywords internal
#' @examples 
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' par(mar=c(1,1,1,1))
#' hatchedLayer(mtq, "dot")
#' title("dot")
#' plot(st_geometry(mtq), border = NA, col="grey80")
#' hatchedLayer(mtq, "text", txt = "Y", add=TRUE)
#' title("text")
#' hatchedLayer(mtq, "diamond", density = 0.5)
#' plot(st_union(st_geometry(mtq)), add = TRUE)
#' title("diamond")
#' hatchedLayer(mtq, "grid", lwd = 1.5)
#' title("grid")
#' hatchedLayer(mtq, "hexagon", col = "blue")
#' title("hexagon")
#' hatchedLayer(mtq, "horizontal", lty = 5)
#' title("horizontal")
#' hatchedLayer(mtq, "vertical")
#' title("vertical")
#' hatchedLayer(mtq, "left2right")
#' title("left2right")
#' hatchedLayer(mtq, "right2left")
#' title("right2left")
#' hatchedLayer(mtq, "zigzag",cellsize=5000)
#' title("zigzag")
#' hatchedLayer(mtq, "circle")
#' title("circle")
#' @export
hatchedLayer <- function(x, pattern = "dot", density = 1, txt = "a", ...) {
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::hatchedLayer()",
                            with = "tc_pattern()") 
  # Assign default options #
  dots <- list(...)
  mode <- ifelse(is.null(dots$mode), "plot", dots$mode)
  col <- ifelse(is.null(dots$col), par()$col, dots$col)
  bg <- ifelse(is.null(dots$bg), par()$bg, dots$bg)
  pch <- ifelse(is.null(dots$pch), par()$pch, dots$pch)
  lty <- ifelse(is.null(dots$lty), par()$lty, dots$lty)
  cex <- ifelse(is.null(dots$cex), par()$cex, dots$cex)
  lwd <- ifelse(is.null(dots$lwd), par()$lwd, dots$lwd)
  add <- ifelse(is.null(dots$add), F, dots$add)
  # End defaults #
  
  
  #Change1: Goal is to create the grid over the devplot
  if (mode == "legend" | add == FALSE) {
    devsfc <- sf::st_as_sfc(sf::st_bbox(x))
  } else {
    devsfc <- par()$usr
    devsfc <- devsfc[c(1, 3, 2, 4)]
    class(devsfc) <- "bbox"
    crsdev <- sf::st_crs(x)
    devsfc <- sf::st_as_sfc(devsfc)
    sf::st_crs(devsfc) <- crsdev
    x <- sf::st_crop(sf::st_geometry(x), devsfc)
    x <- sf::st_union(x)
  }
  
  #End Change1
  
  # Check inputs #
  checkpatterLayer(x, mode, pattern)
  # End check
  
  todot <- c("dot", "text")
  tolines <- c(
    "diamond",
    "grid",
    "hexagon",
    "horizontal",
    "vertical",
    "zigzag",
    "left2right",
    "right2left",
    "circle"
  )
  
  
  # Dimensions #
  # by default 10 cells on the shortest dimensions #
  dist <- min(diff(sf::st_bbox(devsfc)[c(1, 3)]),
              diff(sf::st_bbox(devsfc)[c(2, 4)])) / (10 * density)
  
  # Superseed if cellsize option provided #
  dist <- ifelse(is.null(dots$cellsize), dist, dots$cellsize)
  
  # Prepare to grid #
  if (pattern %in% c("dot", "text")) {
    ops <- list(cellsize = dist ,
                what = "corners",
                square = FALSE)
  } else {
    tops <- pattern != "hexagon"
    ops <- list(cellsize = dist ,
                what = "polygons",
                square = tops)
  }
  if (mode == "legend") {
    fillgrid <- sf::st_make_grid(devsfc,
                                 n = c(3, 3) * density,
                                 what = ops[2],
                                 square = as.logical(ops[3]))
  } else {
    fillgrid <- sf::st_make_grid(
      devsfc,
      cellsize = as.numeric(ops[1]),
      what = ops[2],
      square = as.logical(ops[3])
    )
  }
  # Grid created #
  
  
  # Create patterns #
  # 1. circle #
  if (pattern == "circle") {
    x <- sf::st_union(x)
    centr <- sf::st_centroid(sf::st_geometry(x),
                             of_largest_polygon = TRUE)
    rad <- min(diff(sf::st_bbox(x)[c(1, 3)]),
               diff(sf::st_bbox(x)[c(2, 4)]))
    # by default 21 circles would be created.
    # if cellsize provided the number of circles would be adjusted
    if (mode == "legend") {
      ntimes <- as.integer(2 * density) + 1
    } else if (is.null(dots$cellsize)) {
      ntimes <- as.integer(20 * density) + 1
    } else {
      ntimes <- as.integer(rad / dots$cellsize) + 1
    }
    seg <- rad / ntimes
    # Initial circle #
    lp <- sf::st_buffer(centr, seg / 8)
    lp <- sf::st_cast(lp , "LINESTRING")
    
    for (i in 1:ntimes) {
      join <- sf::st_buffer(centr, dist = seg * i)
      join <- sf::st_cast(join, "LINESTRING")
      lp <- sf::st_union(lp, join)
    }
    endsf <-  sf::st_intersection(lp, x)
  } else if (pattern %in% c("dot", "text")) {
    # 2. dot and text #
    # Buffering around the shp
    x <- sf::st_union(x)
    d <- as.double(sf::st_distance(fillgrid,
                                   sf::st_cast(x, "MULTILINESTRING")))
    endsf <- fillgrid[d > (dist / 4)]
    endsf <- endsf[sf::st_contains(sf::st_union(x) ,endsf, sparse = FALSE)]
    if (pattern == "text") {
      endsf <- sf::st_sf(txt = txt, geometry = endsf)
    } else {
      endsf <- sf::st_union(endsf)
    }
  } else if (pattern %in% c("grid", "hexagon")) {
    # 3. grid and hexagon #
    endsf <- sf::st_cast(fillgrid, "LINESTRING")
    endsf <- sf::st_intersection(endsf, x)
    endsf <- endsf[sf::st_geometry_type(endsf)
                   %in% c("LINESTRING", "MULTILINESTRING")]
    endsf <- sf::st_line_merge(sf::st_union(endsf))
  } else if (!pattern %in% c("zigzag", "diamond")) {
    # 4. rest except zigzag and diamonds #
    ex <- list(
      horizontal = c(1, 2),
      vertical = c(1, 4),
      left2right = c(2, 4),
      right2left = c(1, 3)
    )
    endsf <- lapply(1:length(fillgrid), function(j)
      sf::st_linestring(sf::st_coordinates(fillgrid[j])[ex[[pattern]], 1:2]))
    endsf <- sf::st_sfc(endsf, crs = sf::st_crs(x))
    endsf <- sf::st_intersection(endsf, x)
    endsf <- endsf[sf::st_geometry_type(endsf)
                   %in% c("LINESTRING", "MULTILINESTRING")]
    endsf <- sf::st_line_merge(sf::st_union(endsf))
  } else {
    # 5. zigzag and diamonds #
    l2r <- lapply(1:length(fillgrid), function(j)
      sf::st_linestring(sf::st_coordinates(fillgrid[j])[c(2, 4), 1:2]))
    l2r <- sf::st_sfc(l2r, crs = sf::st_crs(x))
    r2l <- lapply(1:length(fillgrid), function(j)
      sf::st_linestring(sf::st_coordinates(fillgrid[j])[c(1, 3), 1:2]))
    r2l <- sf::st_sfc(r2l, crs = sf::st_crs(x))
    
    if (pattern == "diamond") {
      l2r <- sf::st_line_merge(sf::st_union(l2r))
      r2l <- sf::st_line_merge(sf::st_union(r2l))
      endsf <- sf::st_union(l2r,
                            r2l)
    } else {
      if (mode == "legend") {
        nrows <- 3 * density
        ncols <- 3 * density
      } else {
        ncols <- as.integer(diff(sf::st_bbox(fillgrid)[c(1, 3)]) / (dist))
        nrows <- as.integer(length(fillgrid) / ncols)
      }
      id_grid <- seq(1, length(fillgrid))
      row_id <- cut(id_grid, nrows, labels = FALSE)
      col_id <- id_grid - (row_id - 1) * ncols
      l2r <- l2r[col_id %in% seq(1, ncols + 1, 2)]
      l2r <- sf::st_line_merge(sf::st_union(l2r))
      r2l <- r2l[col_id %in% seq(2, ncols + 1, 2)]
      r2l <- sf::st_line_merge(sf::st_union(r2l))
      endsf <- sf::st_union(l2r,
                            r2l)
    }
    endsf <- sf::st_intersection(endsf, x)
    endsf <- endsf[sf::st_geometry_type(endsf)
                   %in% c("LINESTRING", "MULTILINESTRING")]
    endsf <- sf::st_line_merge(sf::st_union(endsf))
  }
  # End patterns#
  
  #Outputs
  
  # Mode plot: plotting
  # Mode sfc: return object plotted
  # Mode legend: return object to plot on legend
  
  
  if (mode == "plot") {
    if (pattern == "dot") {
      plot(
        endsf,
        add = add,
        col = col,
        bg = bg,
        cex = cex,
        pch = pch
      )
    } else if (pattern == "text") {
      plot(
        sf::st_geometry(x),
        add = add,
        col = NA,
        border = NA
      )
      text(
        x = sf::st_coordinates(endsf)[, 1],
        y = sf::st_coordinates(endsf)[, 2],
        labels = endsf$txt,
        col = col,
        cex = cex
      )
    } else {
      plot(
        sf::st_geometry(endsf),
        add = add,
        col = col,
        lwd = lwd,
        lty = lty
      )
    }
  } else {
    return(endsf)
  }
}


################################################################################
### check utils
#' @name checkpatterLayer
#' @title checkpatterLayer
#' @description check validity hatchedLayer
#' @param x x
#' @param mode mode
#' @param pattern pattern
#' @return Silent or error
#' @noRd
checkpatterLayer <- function(x, mode, pattern) {
  if (length(sf::st_geometry(x)) == 0) {
    stop("No layer added, Check input object")
  }
  if (!mode %in% c("plot", "legend", "sfc")) {
    stop("mode should be  'plot' or 'sfc'")
  }
  geotype = as.character(unique(sf::st_geometry_type(x)))
  if (geotype == "MULTIPOLYGON") {
    errorgeo =  FALSE
  } else if (geotype == "POLYGON") {
    errorgeo = FALSE
  } else {
    errorgeo = TRUE
  }
  if (errorgeo) {
    stop("x should be  MULTIPOLYGON or POLYGON")
  }
  todot = c("dot", "text")
  tolines = c(
    "diamond",
    "grid",
    "hexagon",
    "horizontal",
    "vertical",
    "zigzag",
    "left2right",
    "right2left",
    "circle"
  )
  if (!pattern %in% c(todot, tolines)) {
    stop(paste("Patterns available are",
               gsub(
                 ",",
                 ", ",
                 paste(c(todot, tolines), sep = "", collapse = ",")
               ),
               sep = " "))
  }
}