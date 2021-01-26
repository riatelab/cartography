#' @title Get the optimal position of a legend
#'
#' @description Find the optimal for one or two legends. The optimal position is
#' a position that minimize overlap between a spatial object and a legend
#' @eval my_params(c("x"))
#' @param n number of positions to get (1 or 2)
#'
#' @return A vector of position is retured
#' @export
#'
#' @examples
#' mtq <- tc_import_mtq()
#' tc_get_leg_pos(mtq)
tc_get_leg_pos <- function(x, n = 1){
  if(is.null(grDevices::dev.list())){
    bb <- sf::st_bbox(x)
  }else{
    p <- par("usr")
    bb <- sf::st_bbox(c(xmin = p[1], ymin = p[3],
                        xmax = p[2], ymax = p[4]),
                      crs = sf::st_crs(x))
  }
  g <- sf::st_make_grid(x = sf::st_as_sfc(bb), n = c(3,3), crs = sf::st_crs(x))
  y <- sf::st_union(sf::st_convex_hull(sf::st_geometry(x)))
  z <- sf::st_intersects(g, y)
  ind <- which(unlist(lapply(z, length))==0)
  if( n == 1){
    if (length(ind) > 0){
      pos <- max(ind)
    } else{
      pos <- which.min(sf::st_area(sf::st_intersection(g, y)))
    }
  }
  if(n == 2){
    pos <- c(NA, NA)
    if (length(ind) > 1){
      pos <- sort(ind, decreasing = T)[1:2]
    }else{
      if(length(ind) == 1){
        ii <- rep(NA, 9)
        names(ii) <- 1:9
        ii[ind] <- 0
        couv <- sf::st_area(sf::st_intersection(g, y))
        ii[is.na(ii)] <- couv
        pos <- as.numeric(names(sort(ii)[1:2]))
      }else{
        couv <- sf::st_area(sf::st_intersection(g, y))
        names(couv) <- 1:9
        pos <- as.numeric(names(sort(couv)[1:2]))
      }
    }
  }
  tpos = c("bottomleft", "bottom", "bottomright",
           "left"      , "center", "right",
           "toplef"   , "top"   , "topright")
  return(tpos[pos])
}
