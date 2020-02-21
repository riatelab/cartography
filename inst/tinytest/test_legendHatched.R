library(sf)

mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
plot(st_geometry(mtq))
expect_silent(legendHatched(categ=c("a","b"), patterns = c("dot","diamond"), col="red"))
expect_error(legendHatched(categ=c("a","b"), patterns = c("dot","aa")))
expect_silent(legendHatched(categ=c("a","b","c"), patterns = c("dot","diamond","text"), frame = TRUE, lty = 2))

expect_silent(legendHatched(pos="Err", categ=c("a","b"), patterns = c("dot","diamond")))
