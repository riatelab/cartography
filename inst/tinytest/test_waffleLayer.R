library(sf)
library(sp)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)
mtq$EMP <- mtq$ACT - mtq$CHOM
expect_silent(waffleLayer(x = mtq, var = c("CHOM", "ACT"), cellvalue = 5000))
expect_error(waffleLayer(x = mtq, var = c("CHOM", "ACT"), cellvalue = 5000, cellrnd = "rndd"))
