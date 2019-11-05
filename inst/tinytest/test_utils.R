library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)

df <- st_set_geometry(mtq, NULL)

expect_silent(choroLayer(spdf = as(mtq, "Spatial"), df = df, var = "MED"))




expect_silent(choroLayer(x = mtq, breaks = c(1,2,3,200000), var = "MED"))




expect_silent(typoLayer(x = mtq, var = "LIBGEO"))


expect_error(typoLayer(x = mtq, var = "STATUS", col = c('red', 'blue')))
expect_error(typoLayer(x = mtq, var = "STATUS", col = c('red', 'green', 'blue'),
                       legend.values.order = c("A", "B", "C")))



x <- st_as_sf(data.frame(x = rep(0,10), y = rep(0,10), var = rep('a',10)),
              coords = c("x", "y"), crs = 32620)
plot(st_geometry(x))
expect_silent(labelLayer(x, txt  = "var", overlap = F))  


