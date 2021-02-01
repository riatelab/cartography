#' @title \code{.png} Layer
#' @name getPngLayer
#' @description Get a \code{RasterBrick} from a \code{.png} image cut using the 
#' shape of a spatial object. The \code{.png} file could be either a local file 
#' or extracted from a given url.
#' @param x an \code{sf} object, a simple feature collection (POLYGON or 
#' MULTIPOLYGON) or a tile (see \code{\link{getTiles}}).  
#' @param pngpath local path or url of a \code{.png} file. 
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
#' @param dwmode Set the download mode. It could be \code{'base'} for 
#' \code{\link[utils:download.file]{download.file}} or \code{'curl'} for 
#' \code{\link[curl:curl_download]{curl_download}}.
#' @param ... additional arguments for downloading the file. See 
#' \code{\link[utils:download.file]{download.file}} or 
#' \code{\link[curl:curl_download]{curl_download}}.
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
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @seealso \link{tc_get_png}
#' @examples 
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"))
#' #Local file
#' dirpng <- system.file("img/LogoMartinique.png", package = "cartography")
#' mask <- getPngLayer(mtq, dirpng)
#' 
#' @export
#' @keywords internal
getPngLayer <-  function(x, pngpath, align = "center", margin = 0, crop = FALSE,
                         mask = TRUE, inverse = FALSE, dwmode = "curl", ...) {
  
  lifecycle::deprecate_soft(when = "3.0.0", 
                            what = "cartography::getPngLayer()",
                            with = "tc_get_png()") 
  
  if (class(x)[1] != "RasterBrick") {
    geom <- sf::st_geometry(x)
    x <- sf::st_sf(index = 1:length(geom), geometry = geom)
  }
  crs <- sf::st_crs(x)$proj4string
  if (file.exists(pngpath)) {
    pngRB <- raster::brick(png::readPNG(pngpath) * 255, crs = crs)
  } else {
    # Download
    dirfile <- tempfile(fileext = ".png")
    if (dwmode == "base") {
      download.file(pngpath, dirfile, ...)
    } else if (dwmode == "curl") {
      curl::curl_download(pngpath, dirfile, ...)
    }
    pngRB <- raster::brick(png::readPNG(dirfile) * 255, crs = crs)
  }
  
  if (!align %in% c("left", "right", "center", "top", "bottom")) {
    stop("align should be 'left','right','top', 'bottom' or 'center'")
  }
  
  #Geotagging the raster
  
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
  if (mask & class(x)[1] != "RasterBrick") {
    fig <- raster::mask(fig, x, inverse = inverse)
  }
  
  fig
}

