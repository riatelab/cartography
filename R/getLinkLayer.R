#' @name getLinkLayer
#' @title Create a Links Layer from a Data Frame of Links.
#' @description Create a links layer from a data frame of links.
#' @param x an sf object, a simple feature collection (or a Spatial*DataFrame).
#' @param df a data frame that contains identifiers of starting and ending points.
#' @param xid identifier field in x, default to the first column (optional)
#' @param dfid identifier fields in df, character vector of length 2, default to 
#' the two first columns. (optional)
#' @param spdf defunct.
#' @param spdf2 defunct. 
#' @param spdfid defunct.
#' @param spdf2id defunct.
#' @param dfids defunct.
#' @param dfide defunct.
#' @return An sf LINESTRING is returned, it contains two fields (origins and destinations).
#' @import sp
#' @examples 
#' library(sp)
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
getLinkLayer <- function(x, xid = NULL, df, dfid = NULL, spdf, spdf2 = NULL, 
                         spdfid = NULL, spdf2id = NULL,
                         dfids = NULL, dfide = NULL){
if(sum(missing(spdf), is.null(spdf2), is.null(spdfid), 
       is.null(spdf2id), is.null(dfids), is.null(dfide)) != 6){
  stop("spdf, spdf2, spdfid, spdf2id, dfids and dfide are defunct arguments; last used in version 1.4.2.",
       call. = FALSE)
}
  
  if(methods::is(x, "Spatial")){x <- sf::st_as_sf(x)}
  if(is.null(xid)){xid <- names(x)[1]}
  if (is.null(dfid)){dfid <- names(df)[1:2]}
  x2 <- data.frame(id = x[[xid]], 
                   sf::st_coordinates(sf::st_centroid(x = x, of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))), 
                   stringsAsFactors = F)
  # names(x2)[2:3] <- c('X', 'Y')
  df <- df[,dfid]
  link <- merge(df, x2, by.x = dfid[2], by.y = "id", all.x = TRUE)
  link <- merge(link, x2, by.x =  dfid[1], by.y = "id", all.x = TRUE)
  names(link)[3:6] <- c('xj', 'yj', 'xi', 'yi')

  stringo <- paste0('LINESTRING(', link$xi, " ",link$yi, ", ", 
                    link$xj, " ", link$yj, ")")
  link <- sf::st_sf(link[, dfid], 
                geometry = sf::st_as_sfc(stringo, crs = sf::st_crs(x)))
  return(link)
}