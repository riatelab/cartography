#' @title Wordcloud Layer
#' @name wordcloudLayer
#' @description Plot a word cloud adjusted to an \code{sf} object.
#' @param x an sf object, a simple feature collection (POLYGON or MULTIPOLYGON).  
#' @param txt labels variable.
#' @param freq frequencies of \code{txt}.
#' @param max.words Maximum number of words to be plotted. least frequent terms dropped
#' @param cex.maxmin integer (for same size in all \code{txt}) or vector of length 2 indicating the range of the size of the words.
#' @param rot.per proportion words with 90 degree rotation
#' @param col color or vector of colors words from least to most frequent
#' @param fittopol logical. If true would override \code{rot.per} for some elements of \code{x}
#' @param use.rank logical. If true rank of frequencies is used instead of real frequencies.
#' @param add whether to add the layer to an existing plot (TRUE) or not (FALSE)
#' @param breaks,method,nclass additional arguments for adjusting the colors of \code{txt}, see \code{\link{choroLayer}}.
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @seealso \link{choroLayer}, \link{legendChoro}
#' @references   Ian Fellows (2018). wordcloud: Word Clouds.\cr\cr
#'  R package version 2.6. \url{https://CRAN.R-project.org/package=wordcloud}
#' @examples
#' library(sf) 
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' par(mar=c(0,0,0,0))
#' plot(st_geometry(mtq),
#'      col = "white",
#'      bg = "grey95",
#'      border = NA)
#' wordcloudLayer(
#'   x = mtq,
#'   txt = "LIBGEO",
#'   freq = "POP",
#'   add = TRUE,
#'   nclass = 5
#' )
#' legendChoro(
#'   title.txt = "Population",
#'   breaks = getBreaks(mtq$POP, nclass = 5, method = "quantile"),
#'   col = carto.pal("blue.pal", 5),
#'   nodata = FALSE
#' )
#' @export
wordcloudLayer <- function(x,
                           txt,
                           freq,
                           max.words = NULL,
                           cex.maxmin = c(1, 0.5),
                           rot.per = .1,
                           col = NULL,
                           fittopol = FALSE,
                           use.rank = FALSE,
                           add = FALSE,
                           breaks = NULL,
                           method = "quantile",
                           nclass = NULL) {
  set.seed(999)
  rot.per = max(0.00001, rot.per)
  
  
  #Check
  geomtype = unique(sf::st_geometry_type(x))
  if (!geomtype %in% c("POINT", "POLYGON", "MULTIPOLYGON")) {
    stop("Input should be  MULTIPOLYGON, POLYGON or POINT")
  }
  
  ncoords <- unique(sf::st_coordinates(sf::st_geometry(x))[, 1:2])
  if (nrow(ncoords) < 2 & !add) {
    stop("For single POINT geometries use with add=TRUE")
  }
  
  #Cleaning
  x$text = x[[txt]]
  x$freq = as.double(x[[freq]])
  #Clean up nulls
  x <- x[!is.na(x$freq), ]
  
  x_t1 = x[, c("text", "freq")]
  
  # Work with polygons
  if (geomtype %in% c("POLYGON", "MULTIPOLYGON")) {
    if (fittopol) {
      x_t1$ratiow2h = w2hratio(x_t1)
      x_t1$rot.per = cut(x_t1$ratiow2h,
                         breaks = c(-Inf, 0.8, 1.2, Inf),
                         labels = FALSE)
      x_t1[x_t1$rot.per == 2,]$rot.per <- rot.per
      x_t1[x_t1$rot.per == 1,]$rot.per <- 0.99
      x_t1[x_t1$rot.per == 3,]$rot.per <- 0.01
      x_t1 = x_t1[, c("text", "freq", "rot.per")]
    } else {
      x_t1$rot.per <- rot.per
    }
    geom <-
      sf::st_centroid(sf::st_geometry(x_t1), of_largest_polygon = TRUE)
    x_t1 = sf::st_sf(sf::st_drop_geometry(x_t1), geometry = geom)
  } else {
    x_t1$rot.per <- rot.per
  }
  
  #Sorting
  x_v1 = x_t1[(order(-x_t1$freq)), ]
  
  if (!is.null(max.words)) {
    max.words = min(nrow(x_v1), max.words)
    x_v1 = x_v1[1:max.words, ]
  }
  
  #Normalize
  
  cex.max = max(cex.maxmin)
  cex.min = min(cex.maxmin)
  
  x_v1$normedFreq <- x_v1$freq / max(x_v1$freq)
  x_v1$size <- (cex.max - cex.min) * x_v1$normedFreq + cex.min
  
  
  # Colors
  # get breaks
  layer <-
    choro(
      var = x_v1$freq,
      distr = breaks,
      col = "black",
      nclass = nclass,
      method = method
    )
  breaks = layer$distr
  lencol = length(breaks) - 1
  if (is.null(col)) {
    col <- carto.pal(pal1 = "blue.pal", n1 = lencol)
  } else {
    col = colorRampPalette(col)(lencol)
  }
  x_v1$cut = cut(x_v1$freq,
                 breaks,
                 include.lowest = T,
                 labels = F)
  
  x_v1$col = (col)[x_v1$cut]
  
  #Prepare plot device
  if (!add) {
    lims = sf::st_bbox(sf::st_convex_hull(sf::st_union(x_v1)))
    tobuff = min(lims[3] - lims[1], lims[4] - lims[2]) * 0.1
    buffer = sf::st_buffer(sf::st_as_sfc(lims), tobuff)
    op <- par("mar")
    
    plot.new()
    plot(buffer, col = NA, border = NA)
  }
  
  limdev = par()$usr
  xlim = limdev[1:2]
  ylim = limdev[3:4]
  
  x = sf::st_coordinates(x_v1)[, 1]
  y = sf::st_coordinates(x_v1)[, 2]
  words <- x_v1$text
  cex <- x_v1$size
  col <- x_v1$col
  rotation <- x_v1$rot.per
  
  
  lay = wordcloudlayout(x,
                        y,
                        words,
                        cex,
                        xlim,
                        ylim,
                        rot.per = rotation)
  
  col = col[lay[, "index"]]
  for (i in 1:nrow(lay)) {
    text(
      x = lay[i, "x0"],
      y = lay[i, "y0"],
      labels = row.names(lay)[i],
      cex = lay[i, "cex"],
      srt = lay[i, "rot90"] * 90,
      col = col[i]
    )
    invisible()
  }
  if (!add) {
    par(mar = op)
  }
}
################################################################################
### utils
#' @name wordcloudlayout
#' @title wordcloudlayout
#' @description wordcloudlayout
#' @param x long
#' @param y lat
#' @param words labels
#' @param cex cex
#' @param xlim xlim
#' @param ylim ylim
#' @param tstep tstep
#' @param rstep rstep
#' @param rot.per rot.per
#' @return coords and graphical params
#' @noRd
wordcloudlayout <- function(x,
                            y,
                            words,
                            cex,
                            xlim,
                            ylim,
                            tstep = .1,
                            rstep = .1,
                            rot.per = 0.1) {
  if (length(rot.per) == 1) {
    rot.per = rep(rot.per, length(x))
  }
  tails <- "g|j|p|q|y"
  n <- length(words)
  sdx <- sd(x, na.rm = TRUE)
  sdy <- sd(y, na.rm = TRUE)
  alt = min((xlim[2] - xlim[1]), (ylim[2] - ylim[1])) / 6
  
  if (sdx == 0)
    sdx <- alt
  if (sdy == 0)
    sdy <- alt
  if (length(cex) == 1)
    cex <- rep(cex, n)
  
  boxes <- list()
  additional <- list()
  radiuslim = 10
  for (i in 1:length(words)) {
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- xo <- x[i]
    y1 <- yo <- y[i]
    wid <-
      strwidth(words[i], cex = cex[i]) + 0.1 * strwidth("R", cex = cex[i])
    ht <-
      strheight(words[i], cex = cex[i]) + 0.1 * strheight("R", cex = cex[i])
    
    #mind your ps and qs
    if (grepl(tails, words[i]))
      ht <- ht + ht * .2
    #Fixed on horizontal
    sizetext = c(wid, ht)
    isOverlaped <- TRUE
    col = "red"
    while (isOverlaped) {
      rotWord <- runif(1) < rot.per[i]
      if (rotWord) {
        ht <- sizetext[1]
        wid <- sizetext[2]
      } else {
        ht <- sizetext[2]
        wid <- sizetext[1]
      }
      if (!is_overlap(x1 - .5 * wid, y1 - .5 * ht, wid, ht, boxes) &&
          x1 - .5 * wid > xlim[1] && y1 - .5 * ht > ylim[1] &&
          x1 + .5 * wid < xlim[2] && y1 + .5 * ht < ylim[2]) {
        boxes[[length(boxes) + 1]] <-
          c(x1 - .5 * wid, y1 - .5 * ht, wid, ht)
        additional[[length(additional) + 1]] <-
          c(cex[i], rotWord, i)
        
        isOverlaped <- FALSE
      } else {
        if (r > sqrt(radiuslim)) {
          warning(paste(words[i],
                        "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + tstep
        r <- r + rstep * tstep / (2 * pi)
        x1 <- xo + sdx * r * cos(theta)
        y1 <- yo + sdy * r * sin(theta)
      }
    }
  }
  
  result <- do.call(rbind, boxes)
  colnames(result) <- c("x", "y", "wid", "ht")
  pos <-
    cbind(result[, 1] + .5 * result[, 3], result[, 2] + .5 * result[, 4])
  colnames(pos) <- c("x0", "y0")
  add <- do.call(rbind, additional)
  colnames(add) <- c("cex", "rot90", "index")
  graphpar <-
    matrix(data = (rep(
      c(xlim, ylim, par()$mar), length(result[, 1])
    )),
    ncol = 8,
    byrow = T)
  colnames(graphpar) <- c("usr1", "usr2", "usr3", "usr4",
                          "mar1", "mar2", "mar3", "mar4")
  
  result = cbind(result, pos)
  result = cbind(result, add)
  result = cbind(result, graphpar)
  
  row.names(result) <- words[add[, 3]]
  result
}

#' @name w2hratio
#' @title w2hratio
#' @description w2hratio
#' @param x x
#' @return vector with ratiow2h
#' @noRd

w2hratio <- function(x) {
  #Extract largest polygon
  topol = sf::st_cast(x, "POLYGON", warn = FALSE)
  topol$area <- as.double(sf::st_area(topol))
  maxby = data.frame(max = tapply(topol$area, topol$text, max))
  topolmax = topol[topol$area %in% maxby$max, ]
  
  bboxlist <- lapply(1:nrow(topolmax), function(j)
    as.numeric(sf::st_bbox(topolmax[j, ])))
  ratiow2h <- lapply(1:length(bboxlist), function(s)
    (bboxlist[[s]][3] - bboxlist[[s]][1]) / (bboxlist[[s]][4] - bboxlist[[s]][2]))
  ratiow2h = unlist(ratiow2h)
  ratiow2h
}


#' @importFrom grDevices colorRampPalette
NULL