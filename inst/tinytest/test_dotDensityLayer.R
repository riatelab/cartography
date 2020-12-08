library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
plot(st_geometry(mtq))
expect_silent(dotDensityLayer(x = mtq,  var="POP", pch=20, col = "red4", 
                              legend.frame = FALSE, n = 500))
expect_silent(dotDensityLayer(spdf = as(mtq, "Spatial"), 
                              var="POP", pch=20, col = "red4", 
                              legend.frame = FALSE))
expect_silent(dotDensityLayer(x = mtq,  var="POP", pch=20, 
                              col = "red4", n = 500, add=F))
suppressMessages(expect_error(dotDensityLayer(x = mtq,  var="POP", 
                                                iter = 5, n = 500)))
