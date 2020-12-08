#' Import mtq dataset
#' @name tc_import_mtq
#' @description Import the mtq dataset.
#' @return an sf object
#' @export
#' @importFrom sf st_read
#' @examples
#' mtq <- tc_import_mtq()
tc_import_mtq <- function() {
  st_read(system.file("gpkg/mtq.gpkg", package = "cartography"), quiet = TRUE)
}

