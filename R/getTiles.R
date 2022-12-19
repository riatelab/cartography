#' @title Defunct Get Tiles from Open Map Servers
#' @name getTiles
#' @description 
#' This function is defunct. Use 'maptiles::get_tiles()' instead.
#' 
#' @param x an sf object, a simple feature collection or a Spatial*DataFrame.
#' @param type the tile server from which to get the map. See Details for providers.
#' For other sources use a list: type = list(src = "name of the source" , 
#' q = "tiles address", sub = "subdomains", cit = "how to cite the tiles"). See Examples.
#' @param zoom the zoom level. If null, it is determined automatically 
#' (see Details).
#' @param crop TRUE if results should be cropped to the specified x extent, 
#' FALSE otherwise. If x is an sf object with one POINT, crop is set to FALSE. 
#' @param verbose if TRUE, tiles filepaths, zoom level and citation are displayed. 
#' @param apikey Needed for Thunderforest maps.
#' @param cachedir name of a directory used to cache tiles. If TRUE, places a 
#' 'tile.cache' folder in the working directory. If FALSE, tiles are not cached.
#' @param forceDownload if TRUE, cached tiles are downloaded again. 
#' @details 
#' Zoom levels are described on the OpenStreetMap wiki: 
#' \url{https://wiki.openstreetmap.org/wiki/Zoom_levels}. \cr\cr
#' Full list of providers:  
#' \tabular{lll}{
#' 'OpenStreetMap' (or 'osm')                 \tab	'Stamen' (or 'stamenbw')	                  \tab  'Esri' \cr
#' 'OpenStreetMap.DE'                         \tab	'Stamen.Toner'                            	\tab  'Esri.WorldStreetMap'\cr
#' 'OpenStreetMap.France'                     \tab	'Stamen.TonerBackground'                    \tab  'Esri.DeLorme'\cr
#' 'OpenStreetMap.HOT' (or 'hotstyle')        \tab	'Stamen.TonerHybrid'                        \tab  'Esri.WorldTopoMap'\cr
#'                                            \tab  'Stamen.TonerLines'                         \tab  'Esri.WorldImagery'\cr
#' 'OpenMapSurfer'                            \tab  'Stamen.TonerLabels'                        \tab  'Esri.WorldTerrain'\cr
#' 'OpenMapSurfer.Roads'                      \tab  'Stamen.TonerLite'                          \tab  'Esri.WorldShadedRelief'\cr
#' 'OpenMapSurfer.Hybrid'                     \tab  'Stamen.Watercolor' (or 'stamenwatercolor') \tab  'Esri.OceanBasemap'\cr
#' 'OpenMapSurfer.AdminBounds'                \tab  'Stamen.Terrain'                            \tab  'Esri.NatGeoWorldMap'\cr
#' 'OpenMapSurfer.ElementsAtRisk'             \tab  'Stamen.TerrainBackground'                  \tab  'Esri.WorldGrayCanvas'\cr
#'                                            \tab  'Stamen.TerrainLabels'                      \tab \cr
#' 'CartoDB'                                  \tab                                              \tab  'Hydda'\cr
#' 'CartoDB.Positron' (or 'cartolight')       \tab  'Thunderforest'                             \tab  'Hydda.Full'\cr 
#' 'CartoDB.PositronNoLabels'                 \tab  'Thunderforest.OpenCycleMap'                \tab  'Hydda.Base'\cr
#' 'CartoDB.PositronOnlyLabels'               \tab  'Thunderforest.Transport'                   \tab  'Hydda.RoadsAndLabels'\cr
#' 'CartoDB.DarkMatter' (or 'cartodark')      \tab  'Thunderforest.TransportDark'               \tab \cr 
#' 'CartoDB.DarkMatterNoLabels'               \tab  'Thunderforest.SpinalMap'                   \tab  'HikeBike' (or 'hikebike')\cr
#' 'CartoDB.DarkMatterOnlyLabels'             \tab  'Thunderforest.Landscape'                   \tab  'HikeBike.HikeBike'\cr
#' 'CartoDB.Voyager'                          \tab  'Thunderforest.Outdoors'                    \tab \cr
#' 'CartoDB.VoyagerNoLabels'                  \tab  'Thunderforest.Pioneer'                     \tab  'OpenTopoMap' (or 'opentopomap') \cr
#' 'CartoDB.VoyagerOnlyLabels'                \tab  'Thunderforest.MobileAtlas'                 \tab  'Wikimedia'\cr
#' 'CartoDB.VoyagerLabelsUnder'               \tab  'Thunderforest.Neighbourhood'               \tab  'OpenStreetMap.MapnikBW' (or 'osmgrayscale')\cr
#' }
#' @references \url{https://leaflet-extras.github.io/leaflet-providers/preview/}
#' @export
#' @return A RasterBrick is returned.
#' @seealso \link{tilesLayer}
#' @examples
#' # install.packages('maptiles')
getTiles <- function(x, type = "OpenStreetMap", zoom = NULL, crop = FALSE, 
                     verbose = FALSE, apikey = NA, cachedir = FALSE, 
                     forceDownload = FALSE){
  .Defunct(
    new = "get_tiles", package = "maptiles", 
    msg = "'getTiles' is defunct.\nUse 'maptiles::get_tiles()' instead."
  )
  return(NULL)
}
