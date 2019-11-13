#' @title Get Tiles from Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a spatial object extent. Maps can be 
#' fetched from various open map servers.
#' @param x an sf object, a simple feature collection or a Spatial*DataFrame.
#' @param spdf  deprecated, a Spatial*DataFrame with a valid projection attribute.
#' @param type the tile server from which to get the map, one of "osm", "hotstyle", 
#' "hikebike", "osmgrayscale", "stamenbw", "stamenwatercolor", "cartodark", 
#' "cartolight".
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @param crop TRUE if results should be cropped to the specified x extent, 
#' FALSE otherwise.
#' @param verbose if TRUE, tiles filepaths, zoom level and citation are displayed. 
#' @details 
#' Zoom levels are described on the OpenStreetMap wiki: 
#' \url{http://wiki.openstreetmap.org/wiki/Zoom_levels}.
#' @export
#' @return A RatserBrick is returned.
#' @seealso \link{tilesLayer}
#' @examples
#' \dontrun{
#' library(sf)
#' mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
#' # Download the tiles, extent = Martinique
#' mtqOSM <- getTiles(x = mtq, type = "osm", crop = TRUE)
#' # Plot the tiles
#' tilesLayer(mtqOSM)
#' # Plot countries
#' plot(st_geometry(mtq), add=TRUE)
#' txt <- "\u00A9 OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright"
#' mtext(text = txt, side = 1, adj = 0, cex = 0.7, font = 3)
#' }
getTiles <- function(x, spdf, type = "osm", zoom = NULL, crop = FALSE, 
                     verbose = FALSE){
  
  if(!missing(spdf)){
    warning("spdf is deprecated; use x instead.", call. = FALSE)
    x <- spdf
  }
  
  if(methods::is(x,"Spatial") == TRUE){
    x <- convertToSf(spdf = x)
  }
  
  # use x bbox to select the tiles to get 
  bbx <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(x)), 4326))
  # select a default zoom level
  if(is.null(zoom)){
    gz <- slippymath::bbox_tile_query(bbx)
    zoom <- min(gz[gz$total_tiles %in% 4:10,"zoom"])
  }
  # get tile list
  tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbx, zoom = zoom)
  
  # get query parameters according to type 
  param <- get_param(type)
  # subdomains management
  tile_grid$tiles$s <- sample(param$sub, nrow(tile_grid$tiles), replace = T)
  # extension management 
  tile_grid$ext <- substr(param$q, nchar(param$q)-2, nchar(param$q))
  # src mgmnt
  tile_grid$src <- type
  # query mgmnt
  tile_grid$q <- param$q
  # citation
  tile_grid$cit <- param$cit
  
  # download images
  images <- get_tiles(tile_grid, verbose)
  # compose images
  rout <- compose_tile_grid(tile_grid, images)
  
  # reproject if x is projected
  pref <- list(
    3857,
    "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs",
    "+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +nadgrids=@null +units=m +no_defs"
  )
  
  if (length(intersect(pref,  sf::st_crs(x))) == 0) {
    rout <- raster::projectRaster(from = rout, crs = raster::crs(raster::raster(x)))
    rout <- raster::clamp(rout,lower = 0, upper = 255, useValues = TRUE)
  }
  
  # crop management  
  if(crop == TRUE){
    cb <- sf::st_bbox(x)
    k <- min(c(0.052 * (cb[4] - cb[2]), 0.052 * (cb[3] - cb[1])))
    cb <- cb + c(-k, -k, k, k)
    rout <- raster::crop(rout,cb[c(1,3,2,4)])
  }
  
  rout
}



get_tiles <- function(tile_grid, verbose) {
  images <- apply(
    X = tile_grid$tiles,
    MARGIN = 1, 
    FUN = dl_t,
    z = tile_grid$zoom,
    ext = tile_grid$ext,
    src = tile_grid$src,
    q = tile_grid$q,
    verbose = verbose
  )
  
  if (verbose) {
    cat("Zoom:", tile_grid$zoom, "\nData and map tiles sources:\n",
        tile_grid$cit, "\n")
  }
  images
}

dl_t <- function(x, z, ext, src, q, verbose) {
  outfile <- paste0(tempdir(), "/", src, "_", z, "_", x[1], "_", x[2],".", ext)
  if (!file.exists(outfile)) {
    q <- gsub(pattern = '{s}', replacement = x[3], x = q, fixed = TRUE)
    q <- gsub(pattern = '{x}', replacement = x[1], x = q, fixed = TRUE)
    q <- gsub(pattern = '{y}', replacement = x[2], x = q, fixed = TRUE)
    q <- gsub(pattern = '{z}', replacement = z, x = q, fixed = TRUE)
    if (verbose) {
      cat(q, " => ", outfile, '\n')
    }
    curl::curl_download(url = q, destfile = outfile)
  }
  outfile
} 


compose_tile_grid <- function (tile_grid, images){
  bricks = vector("list", nrow(tile_grid$tiles))
  for (i in seq_along(bricks)){
    bbox <- slippymath::tile_bbox(tile_grid$tiles$x[i], tile_grid$tiles$y[i],
                                  tile_grid$zoom)
    img <- images[i]
    if (tile_grid$ext=="png"){
      img <- png::readPNG(img)*255
    }
    r_img <- raster::brick(img, crs = attr(bbox, "crs")$proj4string)
    raster::extent(r_img) <- raster::extent(bbox[c("xmin", "xmax", "ymin", "ymax")])
    bricks[[i]] <- r_img
  }
  
  if(length(bricks)==1){
    return(bricks[[1]])
  }
  rout <- do.call(raster::merge, bricks)
  rout
}

get_param <- function(type){
  param <- switch(
    type,
    osm = list(
      q = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
      sub = c("a", "b", "c"),
      cit = "\u00A9 OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright."
    ),
    hotstyle = list(
      q = 'http://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png',
      sub = c("a", "b"),
      cit = "\u00A9 OpenStreetMap contributors. Tiles style by Humanitarian OpenStreetMap Team, under CC0, www.hotosm.org."
    ),
    hikebike = list(
      q = "https://tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png",
      sub = NA, 
      cit = "\u00A9 OpenStreetMap contributors. Tiles style under CC0, hikebikemap.net."
    ),
    osmgrayscale = list(
      q = "https://tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png" ,
      sub = NA, 
      cit = "\u00A9 OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright."
    ),
    stamenbw = list(
      q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.png", 
      sub = c("a", "b", "c", "d"), 
      cit = "\u00A9 OpenStreetMap contributors. Tiles style by Stamen Design, under CC BY 3.0, stamen.com."
    ),
    stamenwatercolor =  list(
      q = "https://stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.jpg",
      sub = c("a", "b", "c", "d"), 
      cit = "\u00A9 OpenStreetMap contributors. Tiles style by Stamen Design, under CC BY 3.0, stamen.com."
    ),
    cartodark = list(
      q = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
      sub = c("a", "b", "c", "d"), 
      cit = "\u00A9 OpenStreetMap contributors. Tiles style by Carto, under CC BY 3.0, carto.com/attribution."
    ),
    cartolight = list(
      q = 'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png',
      sub = c("a", "b", "c", "d"), 
      cit = "\u00A9 OpenStreetMap contributors. Tiles style by Carto, under CC BY 3.0, carto.com/attribution."
    )
  )
  param
}


