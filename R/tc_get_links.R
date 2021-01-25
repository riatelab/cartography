#' @name tc_get_links
#' @title Get a link layer from a data.frame of links.
#' @description Create a links layer from a data.frame of links and an sf object.
#' @param x an sf object, a simple feature collection.
#' @param df a data.frame that contains identifiers of starting and ending points.
#' @param x_id name of the identifier variable in x, default to the first column (optional)
#' @param df_id names of the identifier variables in df, character vector of length 2, default to 
#' the two first columns. (optional)
#' @return An sf object is returned, it is composed of df and the sfc (LINESTRING) of links.
#' @examples
#' mtq <- tc_import_mtq()
#' mob <- read.csv(system.file("csv/mob.csv", package="cartography"))
#' # Select links from Fort-de-France (97209))
#' mob_97209 <- mob[mob$i == 97209, ]
#' # Create a link layer
#' mob_links <- tc_get_links(x = mtq, df = mob_97209)
#' # Plot the links
#' tc_map(mtq)
#' tc_map(mob_links, col = "red4", lwd = 2, add = TRUE)
#' @export
tc_get_links <- function(x, df, x_id, df_id){
  if (missing(x_id)){x_id <- names(x)[1]}
  if (missing(df_id)){df_id <- names(df)[1:2]}
  # extract x centroids
  x2 <- data.frame(
    id = x[[x_id]],
    sf::st_coordinates(sf::st_centroid(x = sf::st_geometry(x), of_largest_polygon = TRUE)), 
    stringsAsFactors = FALSE
  )
  link <- merge(df, x2, by.x = df_id[2], by.y = "id", all.x = TRUE)
  link <- merge(link, x2, by.x =  df_id[1], by.y = "id", all.x = TRUE)
  # watch for NAs
  d1 <- nrow(link)
  link <- link[!is.na(link$X.x) & !is.na(link$X.y),]
  d2 <- nrow(link)
  if(d2 == 0){
    stop("No links were created. df_id and x_id do not match.", call. = FALSE)
  }
  if((d1 - d2) > 0){
    warning(paste0((d1 - d2), 
                   " links were not created. Some ids from df were not found in x."), 
            call. = FALSE)
  }
  # build the link
  stringo <- paste0('LINESTRING(', link$X.y, " ",link$Y.y, ", ", 
                    link$X.x, " ", link$Y.x, ")")
  link <- sf::st_sf(link[, 1:ncol(df)], 
                    geometry = sf::st_as_sfc(stringo, crs = sf::st_crs(x)))
  return(link)
}

