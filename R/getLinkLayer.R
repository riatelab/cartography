#' @name getLinkLayer
#' @title Create a SpatialLinesDataFrame from a Data Frame of Links.
#' @description Create a SpatialLinesDataFrame from a data frame of links.
#' @param x ...
#' @param df a data frame that contains identifiers of starting and ending points.
#' @param xid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in spdf2, default to the first column 
#' of the spdf2 data frame. (optional)
#' @return A SpatialLinesDataFrame is returned, its data frame contains two fields (dfids and dfide).
#' @import sp
#' @examples 
#' data("nuts2006")
#' # Create a link layer
#' head(twincities.df)
#' # Select links from Ireland (IE)
#' twincitiesIE <- twincities.df[substr(twincities.df$i,1,2)=="IE", ]
#' twincities.sf <- getLinkLayer(x = nuts2.spdf, df = twincitiesIE, dfid = c("i", "j"))
#' # Plot the links
#' plot(nuts2.spdf, col = "#6C6870")
#' plot(twincities.sf, col = "#F78194", add = TRUE)
#' @seealso \link{gradLinkLayer}, \link{propLinkLayer}
#' @export
getLinkLayer <- function(x, xid = NULL, df, dfid = NULL){
  if(methods::is(x, "Spatial")){x <- sf::st_as_sf(x)}
  if(is.null(xid)){xid <- names(x)[1]}
  if (is.null(dfid)){dfid <- names(df)[1:2]}
  x2 <- data.frame(id = x[[xid]], 
                   do.call(rbind, sf::st_geometry(sf::st_centroid(x))), 
                   stringsAsFactors = F)
  names(x2)[2:3] <- c('X', 'Y')
  df <- df[,dfid]
  link <- merge(df, x2, by.x = dfid[2], by.y = "id", all.x = TRUE)
  link <- merge(link, x2, by.x =  dfid[1], by.y = "id", all.x = TRUE)
  names(link)[3:6] <- c('xj', 'yj', 'xi', 'yi')

  stringo <- paste0('LINESTRING(', link$xi, " ",link$yi, ", ", 
                    link$xj, " ", link$yj, ")")
  link <- sf::st_sf(link[,c('i','j')], 
                geometry = sf::st_as_sfc(stringo, crs = sf::st_crs(x)))
  return(link)
}