library(sf)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)


#Local file
dirpng = system.file("img/LogoMartinique.png", package = "cartography")


xtest=getPngLayer(mtq,dirpng)
expect_silent(pngLayer(xtest))
expect_error(pngLayer(mtq))
