library(sf)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "cartography"), quiet = TRUE)

home <- length(unclass(packageVersion("cartography"))[[1]]) == 4

if (home) {#Remote
  remoteurl <- "https://i.imgur.com/gePiDvB.png"
  expect_true(
    methods::is(getPngLayer(x = mtq,pngpath = remoteurl,dwmode = "curl"),
                "RasterBrick"))
  expect_true(
    methods::is(getPngLayer(x = mtq,pngpath = remoteurl,dwmode = "base",
                            mode = "wb", quiet = TRUE),
                "RasterBrick"))
}

#Local file
dirpng <-
  system.file("img/LogoMartinique.png", package = "cartography")
expect_true(methods::is(getPngLayer(x = mtq, pngpath = dirpng), "RasterBrick"))

expect_error(getPngLayer(x = mtq, pngpath = dirpng, align = "fake"))
expect_silent(getPngLayer(mtq, dirpng))
expect_silent(getPngLayer(mtq, dirpng, margin = 0.2, crop = TRUE))
expect_silent(getPngLayer(mtq, dirpng, align = "left"))
expect_silent(getPngLayer(mtq, dirpng, align = "right"))
expect_error(getPngLayer(x = mtq, pngpath = "https://i.imgur.com/2CJpz98.jpg"))
# mtq.car <- getTiles(x = mtq, type = "cartodark", crop = TRUE)
# expect_silent(getPngLayer(mtq.car, dirpng, crop = TRUE))

mtqhoriz <- mtq[mtq$INSEE_COM %in% c(97230, 97234), ]
expect_silent(getPngLayer(mtqhoriz, dirpng, align = "top", mask = FALSE))
expect_silent(getPngLayer(mtqhoriz, dirpng, align = "bottom", mask = FALSE))


