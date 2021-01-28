#' @title Get a RasterBrick from a png file
#' @name tc_get_png
#' @description Get a \code{RasterBrick} from a \code{.png} image cut using the 
#' shape of a spatial object. 
#' @param x an \code{sf} object, a simple feature collection (POLYGON or 
#' MULTIPOLYGON).  
#' @param file path to the file. 
#' @param align set how the \code{.png} file should be fitted within \code{x}.
#'  Possible values are \code{'left','right','top', 'bottom'} or \code{'center'}.
#' @param margin inner margin, zooms out the \code{.png} over \code{x}. If 0
#'  then \code{.png} is completely zoomed over \code{x}.
#' @param crop \code{TRUE} if results should be cropped to the specified 
#' \code{x} extent.
#' @param mask \code{TRUE} if the result should be masked to \code{x}.
#' @param inverse  logical. If \code{FALSE}, overlapped areas of \code{x} on 
#' \code{pngpath} are extracted, otherwise non-overlapping areas are returned. 
#' See \code{\link[raster:mask]{mask}}.
#' @return A \code{RasterBrick} object is returned. 
#' @details The effect of \code{align} would differ depending of the aspect 
#' ratio of \code{x} and \code{pngpath}. To obtain a fitted tile from 
#' \code{pngpath} 
#' given that \code{x} is the tile to fit, set \code{margin = 0 , crop = TRUE}.
#' @note The accuracy of the final plot would depend on the quality of the 
#' \code{.png} file, 
#' the scale of \code{x} and the resolution setup of the graphic device. 
#' Exporting to \code{svg} is highly
#' recommended.
#' @seealso \link{tc_map_r}
#' @examples 
#' mtq <- tc_import_mtq()
#' file_path <- system.file("img/LogoMartinique.png", package = "cartography")
#' logo <- tc_get_png(x = mtq, file = file_path)
#' @export
tc_get_png <-  function(x, file, align = "center", margin = 0, crop = FALSE,
                         mask = TRUE, inverse = FALSE) {

  if (!align %in% c("left", "right", "center", "top", "bottom")) {
    stop("align should be 'left','right','top', 'bottom' or 'center'")
  }
  
  #Geotagging the raster
  img <- png::readPNG(file) * 255

  # Adding transparency
  if (dim(img)[3] == 4) {
    nrow <- dim(img)[1]
    for (j in seq_len(nrow)) {
      row <- img[j, , ]
      alpha <- row[, 4] == 0
      row[alpha, ] <- NA
      img[j, , ] <- row
    }
  }
  
  #Geotagging the raster
  x <- sf::st_as_sf(x)
  crs <- sf::st_crs(x)$proj4string
  pngRB <- raster::brick(img, crs = crs)
  
  
  #Add margin
  extshpinit <- raster::extent(x)
  innermarg <- min((extshpinit@xmax - extshpinit@xmin),
                   (extshpinit@ymax - extshpinit@ymin)) * margin
  extshp <- extshpinit + innermarg
  
  #Relation w2h
  ratiopng <- dim(pngRB)[2] / dim(pngRB)[1]
  wfig <- dim(pngRB)[2]
  hfig <- dim(pngRB)[1]
  
  ratiox <- (extshp@xmax - extshp@xmin) / (extshp@ymax - extshp@ymin)
  w <- (extshp@xmax - extshp@xmin)
  h <- (extshp@ymax - extshp@ymin)
  w_mp <- extshp@xmin + w / 2
  h_mp <- extshp@ymin + h / 2
  ev <- as.double(extshp[])
  if (ratiox > ratiopng) {
    if (align == "top") {
      new_ext <- c(ev[1], ev[2], ev[4] - w / ratiopng, ev[4])
    } else if (align == "bottom") {
      new_ext <- c(ev[1], ev[2], ev[3], ev[3] + w / ratiopng)
    } else {
      new_ext <- c(ev[1], ev[2], h_mp - 0.5 * w / ratiopng, h_mp + 0.5 * w / ratiopng)
    }
  } else {
    if (align == "left") {
      new_ext <- c(ev[1], ev[1] + h * ratiopng, ev[3], ev[4])
    } else if (align == "right") {
      new_ext <- c(ev[2] - h * ratiopng, ev[2], ev[3], ev[4])
    } else {
      new_ext <- c(w_mp - 0.5 * h * ratiopng, w_mp + 0.5 * h * ratiopng, ev[3], ev[4])
    }
  }
  raster::extent(pngRB) <- new_ext
  fig <- pngRB
  
  # Crop
  if (crop) {
    fig <- raster::crop(fig, extshpinit)
  }
  
  # Mask
  if (mask) {
    fig <- raster::mask(fig, x, inverse = inverse)
  }
  
  fig
}

