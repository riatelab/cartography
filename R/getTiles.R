#' @title Get Tiles from Open Map Servers
#' @name getTiles
#' @description Get map tiles based on a spatial object extent. Maps can be 
#' fetched from various open map servers.
#' @param x an sf object, a simple feature collection or a Spatial*DataFrame.
#' @param spdf  deprecated, a Spatial*DataFrame with a valid projection attribute.
#' @param type the tile server from which to get the map, one of "osm", "hotstyle", 
#' "hikebike", "osmgrayscale", "stamenbw", "stamenwatercolor", "cartodark", 
#' "cartolight", "opentopomap". For other sources use a list: 
#' type = list(src="name of the source" , q = "tiles address", sub = "subdomains", 
#' cit = "how to cite the tiles"). See Examples.
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @param crop TRUE if results should be cropped to the specified x extent, 
#' FALSE otherwise. If x is an sf object with one POINT, crop is set to FALSE. 
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
#' txt <- paste0("\u00A9 OpenStreetMap contributors.", 
#'               " Tiles style under CC BY-SA, www.openstreetmap.org/copyright")
#' mtext(text = txt, side = 1, adj = 0, cex = 0.7, font = 3)
#' 
#' # Download esri tiles
#' typeosm <-  list(
#'   src = 'esri',
#'   q = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg',
#'   sub = NA,
#'   cit = 'Tiles; Esri; Source: Esri, DeLorme, NAVTEQ, USGS, Intermap, iPC, NRCAN, Esri Japan, METI, Esri China (Hong Kong), Esri (Thailand), TomTom, 2012'
#' )
#' mtqESRI <- getTiles(x = mtq, type = typeosm, crop = TRUE, verbose = T, zoom = 10)
#' # Plot the tiles
#' tilesLayer(mtqESRI)
#' txt <- typeosm$cit
#' mtext(text = txt, side = 1, adj = 0, cex = 0.6, font = 3)
#' }
getTiles <- function(x, spdf, type = "osm", zoom = NULL, crop = FALSE, 
                     verbose = FALSE){
  # deprecated check
  if(!missing(spdf)){
    warning("spdf is deprecated; use x instead.", call. = FALSE)
    x <- spdf
  }
  # test for sp
  if(methods::is(x,"Spatial") == TRUE){
    x <- convertToSf(spdf = x)
  }
  # test for single point (apply buffer to obtain a correct bbox)
  if(nrow(x)==1){
    if(sf::st_is(x, "POINT")){
      xt <- sf::st_transform(x, 3857)
      sf::st_geometry(xt) <- sf::st_buffer(sf::st_geometry(xt), 1000)
      crop <- FALSE
      # use x bbox to select the tiles to get 
      bbx <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(xt)), 4326))
    }
  }else{
    # use x bbox to select the tiles to get 
    bbx <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(x)), 4326))
  }
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
  tile_grid$src <- param$src
  # query mgmnt
  tile_grid$q <- param$q
  # citation
  tile_grid$cit <- param$cit
  
  # download images
  images <- get_tiles(tile_grid, verbose)
  # compose images
  rout <- compose_tile_grid(tile_grid, images)
  
  # reproject rout
  rout <- raster::projectRaster(from = rout, crs = sf::st_crs(x)$proj4string)
  rout <- raster::clamp(rout,lower = 0, upper = 255, useValues = TRUE)
  
  # crop management  
  if(crop == TRUE){
    cb <- sf::st_bbox(x)
    k <- min(c(0.052 * (cb[4] - cb[2]), 0.052 * (cb[3] - cb[1])))
    cb <- cb + c(-k, -k, k, k)
    rout <- raster::crop(rout,cb[c(1,3,2,4)])
  }
  
  rout
}


# get the tiles according to the grid
get_tiles <- function(tile_grid, verbose) {
  # go through tile_grid tiles and download
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
    message("Zoom:", tile_grid$zoom, "\nData and map tiles sources:\n",
            tile_grid$cit)
  }
  images
}

# download tile according to parameters
dl_t <- function(x, z, ext, src, q, verbose) {
  outfile <- paste0(tempdir(), "/", src, "_", z, "_", x[1], "_", x[2],".", ext)
  if (!file.exists(outfile)) {
    q <- gsub(pattern = '{s}', replacement = x[3], x = q, fixed = TRUE)
    q <- gsub(pattern = '{x}', replacement = x[1], x = q, fixed = TRUE)
    q <- gsub(pattern = '{y}', replacement = x[2], x = q, fixed = TRUE)
    q <- gsub(pattern = '{z}', replacement = z, x = q, fixed = TRUE)
    if (verbose) {
      message(q, " => ", outfile)
    }
    curl::curl_download(url = q, destfile = outfile)
  }
  outfile
} 

# compose tiles 
compose_tile_grid <- function (tile_grid, images){
  bricks = vector("list", nrow(tile_grid$tiles))
  for (i in seq_along(bricks)){
    bbox <- slippymath::tile_bbox(tile_grid$tiles$x[i], tile_grid$tiles$y[i],
                                  tile_grid$zoom)
    img <- images[i]
    # special for png tiles
    if (tile_grid$ext=="png"){
      img <- png::readPNG(img)*255
    }
    # compose brick raster
    r_img <- raster::brick(img, crs = attr(bbox, "crs")$proj4string)
    raster::extent(r_img) <- raster::extent(bbox[c("xmin", "xmax", "ymin", "ymax")])
    bricks[[i]] <- r_img
  }
  # if only one tile is needed
  if(length(bricks)==1){
    return(bricks[[1]])
  }
  # all tiles together
  rout <- do.call(raster::merge, bricks)
  rout
}

# providers parameters
get_param <- function(type){
  if (length(type) == 4){
    param <- type
  }else{
    param <- switch(
      type,
      osm = list(
        src = "osm",
        q = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
        sub = c("a", "b", "c"),
        cit = "\u00A9 OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright."
      ),
      hotstyle = list(
        src = "hotstyle",
        q = 'http://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png',
        sub = c("a", "b"),
        cit = "\u00A9 OpenStreetMap contributors. Tiles style by Humanitarian OpenStreetMap Team, under CC0, www.hotosm.org."
      ),
      hikebike = list(
        src = "hikebike",
        q = "https://tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png",
        sub = NA, 
        cit = "\u00A9 OpenStreetMap contributors. Tiles style under CC0, hikebikemap.net."
      ),
      osmgrayscale = list(
        src = "osmgrayscale",
        q = "https://tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png" ,
        sub = NA, 
        cit = "\u00A9 OpenStreetMap contributors. Tiles style under CC BY-SA, www.openstreetmap.org/copyright."
      ),
      stamenbw = list(
        src = "stamenbw",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.png", 
        sub = c("a", "b", "c", "d"), 
        cit = "\u00A9 OpenStreetMap contributors. Tiles style by Stamen Design, under CC BY 3.0, stamen.com."
      ),
      stamenwatercolor =  list(
        src = "stamenwatercolor",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.jpg",
        sub = c("a", "b", "c", "d"), 
        cit = "\u00A9 OpenStreetMap contributors. Tiles style by Stamen Design, under CC BY 3.0, stamen.com."
      ),
      cartodark = list(
        src = "cartodark",
        q = 'http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        sub = c("a", "b", "c", "d"), 
        cit = "\u00A9 OpenStreetMap contributors. Tiles style by Carto, under CC BY 3.0, carto.com/attribution."
      ),
      cartolight = list(
        src = "cartolight",
        q = 'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png',
        sub = c("a", "b", "c", "d"), 
        cit = "\u00A9 OpenStreetMap contributors. Tiles style by Carto, under CC BY 3.0, carto.com/attribution."
      ), 
      opentopomap = list(
        src = "opentopomap",
        q = 'https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png',
        sub = c("a", "b", "c"), 
        cit = "\u00A9 OpenStreetMap contributors and SRTM. Tiles style by OpenTopoMap, under CC BY-SA 3.0, opentopomap.org."
      ),
      OpenStreetMap = list(
        src="OpenStreetMap",
        q = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
        sub = c("a", "b", "c"),
        cit = "\u00A9 OpenStreetMap contributors"),
      OpenStreetMap.DE = list(
        src="OpenStreetMap.DE",
        q = "https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "\u00A9 OpenStreetMap contributors"),
      OpenStreetMap.France = list(
        src="OpenStreetMap.France",
        q = "https://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "\u00A9 Openstreetmap France | \u00A9 OpenStreetMap contributors"),
      OpenStreetMap.HOT = list(
        src="OpenStreetMap.HOT",
        q = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "\u00A9 OpenStreetMap contributors, Tiles style by OpenStreetMap France"),
      OpenTopoMap = list(
        src="OpenTopoMap",
        q = "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "Map data: \u00A9 OpenStreetMap contributors, OpenTopoMap (CC-BY-SA)"),
      OpenMapSurfer = list(
        src="OpenMapSurfer",
        q = "https://maps.heigit.org/openmapsurfer/tiles/roads/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data "),
      OpenMapSurfer.Roads = list(
        src="OpenMapSurfer.Roads",
        q = "https://maps.heigit.org/openmapsurfer/tiles/roads/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data \u00A9 OpenStreetMap contributors"),
      OpenMapSurfer.Hybrid = list(
        src="OpenMapSurfer.Hybrid",
        q = "https://maps.heigit.org/openmapsurfer/tiles/hybrid/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data \u00A9 OpenStreetMap contributors"),
      OpenMapSurfer.AdminBounds = list(
        src="OpenMapSurfer.AdminBounds",
        q = "https://maps.heigit.org/openmapsurfer/tiles/adminb/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data \u00A9 OpenStreetMap contributors"),
      OpenMapSurfer.ElementsAtRisk = list(
        src="OpenMapSurfer.ElementsAtRisk",
        q = "https://maps.heigit.org/openmapsurfer/tiles/elements_at_risk/webmercator/{z}/{x}/{y}.png",
        sub = NA,
        cit = "Imagery from GIScience Research Group \uFE6B University of Heidelberg | Map data \u00A9 OpenStreetMap contributors"),
      Hydda = list(
        src="Hydda",
        q = "https://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "Tiles courtesy of OpenStreetMap Sweden \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Hydda.Full = list(
        src="Hydda.Full",
        q = "https://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "Tiles courtesy of OpenStreetMap Sweden \u2014 Map data \u00A9 OpenStreetMap contributors"
      ),
      Hydda.Base = list(
        src="Hydda.Base",
        q = "https://{s}.tile.openstreetmap.se/hydda/base/{z}/{x}/{y}.png",
        sub = c("a", "b", "c"),
        cit = "Tiles courtesy of OpenStreetMap Sweden \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Hydda.RoadsAndLabels = list(
        src="Hydda.RoadsAndLabels",
        q = "https://{s}.tile.openstreetmap.se/hydda/roads_and_labels/{z}/{x}/{y}.png",
        sub  = c("a", "b", "c"),
        cit = "Tiles courtesy of OpenStreetMap Sweden \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen = list(
        src="Stamen",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.Toner = list(
        src="Stamen.Toner",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.TonerBackground = list(
        src="Stamen.TonerBackground",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-background/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.TonerHybrid = list(
        src="Stamen.TonerHybrid",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-hybrid/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.TonerLines = list(
        src="Stamen.TonerLines",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lines/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.TonerLabels = list(
        src="Stamen.TonerLabels",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.TonerLite = list(
        src="Stamen.TonerLite",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.Watercolor = list(
        src="Stamen.Watercolor",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.jpg",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.Terrain = list(
        src="Stamen.Terrain",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/terrain/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.TerrainBackground = list(
        src="Stamen.TerrainBK",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/terrain-background/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Stamen.TerrainLabels = list(
        src="Stamen.Terrainlabs",
        q = "https://stamen-tiles-{s}.a.ssl.fastly.net/terrain-labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "Map tiles by CC BY 3.0 \u2014 Map data \u00A9 OpenStreetMap contributors"),
      Esri = list(
        src="Esri",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}",
        sub = NA,
        cit = "Tiles \u00A9 Esri"),
      Esri.WorldStreetMap = list(
        src="EsriWSM",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Source: Esri, DeLorme, NAVTEQ, USGS, Intermap, iPC, NRCAN, Esri Japan, METI, Esri China (Hong Kong), Esri (Thailand), TomTom, 2012"),
      Esri.DeLorme = list(
        src="EsriDLor",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/Specialty/DeLorme_World_Base_Map/MapServer/til/{z}/{y}/{x}",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Copyright: \u00A92012 DeLorme"),
      Esri.WorldTopoMap = list(
        src="EsriWTM",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}", 
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community"),
      Esri.WorldImagery = list(
        src="EsriWI",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", 
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"),
      Esri.WorldTerrain = list(
        src="EsriWT",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{z}/{y}/{x}",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Source: USGS, Esri, TANA, DeLorme, and NPS"),
      Esri.WorldShadedRelief = list(
        src="EsriWSR",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}", 
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Source: Esri"),
      Esri.OceanBasemap = list(
        src="EsriOBM",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}", 
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"),
      Esri.NatGeoWorldMap = list(
        src="EsriNGW",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"),
      Esri.WorldGrayCanvas = list(
        src="EsriWGC",
        q = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
        sub = NA,
        cit = "Tiles \u00A9 Esri \u2014 Esri, DeLorme, NAVTEQ"),
      CartoDB = list(
        src="Carto",
        q = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),   
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.Positron = list(
        src="CartoP",
        q = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.PositronNoLabels = list(
        src="CartoPNL",  
        q = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.PositronOnlyLabels = list(
        src="CartoPOL",
        q = "https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.DarkMatter = list(
        src="CartoDM",
        q = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.DarkMatterNoLabels = list(
        src="CartoDMNL",
        q = "https://{s}.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.DarkMatterOnlyLabels = list(
        src="CartoDMOL",
        q = "https://{s}.basemaps.cartocdn.com/dark_only_labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.Voyager = list(
        src="CartoV",
        q = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.VoyagerNoLabels = list(
        src="CartoVNL",
        q = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_nolabels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.VoyagerOnlyLabels = list(
        src="CartoVOL",
        q = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_only_labels/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      CartoDB.VoyagerLabelsUnder = list(
        src="CartoVLU",
        q = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png",
        sub = c("a", "b", "c", "d"),
        cit = "\u00A9 OpenStreetMap contributors \u00A9 CARTO"),
      HikeBike = list(
        src="HikeBike",
        q="https://tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png", 
        sub =NA,
        cit="\u00A9 OpenStreetMap contributors"),
      HikeBike.HikeBike = list(
        src="HikeBike2",
        q="https://tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png",
        sub =NA,
        cit="\u00A9 OpenStreetMap contributors"),
      Wikimedia = list(
        src="Wikimedia",
        q="https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}.png",
        sub =NA,
        cit="Wikimedia")
      
    )
  }
  param
}
