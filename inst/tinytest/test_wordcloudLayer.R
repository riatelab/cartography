library(sf)

mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"), quiet = TRUE)


plot(st_geometry(mtq))
expect_silent(wordcloudLayer(mtq, txt ="LIBGEO", freq = "POP", add=TRUE, nclass = 5))
expect_silent(wordcloudLayer(mtq, txt ="LIBGEO", freq = "POP", fittopol = TRUE))
expect_warning(wordcloudLayer(mtq, txt ="LIBGEO", freq = "POP",breaks=c(0,5000,10000), cex.maxmin = c(5,3)))
mtql=st_cast(st_cast(st_geometry(mtq),"POLYGON"),"LINESTRING")
expect_error(wordcloudLayer(mtql, txt ="LIBGEO", freq = "POP", cex.maxmin = c(5,3)))

expect_silent(wordcloudLayer(mtq, txt ="LIBGEO", freq = "POP", col=carto.pal("blue.pal",4)))
expect_silent(wordcloudLayer(mtq, txt ="LIBGEO", freq = "POP", nclass=8,col=carto.pal("blue.pal",4)))
expect_silent(wordcloudLayer(mtq, txt ="LIBGEO", freq = "POP", max.words = 15, col=carto.pal("blue.pal",4)))

expect_silent(wordcloudLayer(mtq, txt ="LIBGEO", freq = "POP", use.rank = TRUE, nclass=5, col=c("yellow","green")))



points = st_sample(mtq, size = nrow(mtq))
mtqp = st_sf(st_drop_geometry(mtq), points)

expect_silent(wordcloudLayer(mtqp, txt ="LIBGEO", freq = "POP", max.words = 15, col=carto.pal("blue.pal",4)))


expect_warning(wordcloudLayer(mtq, txt ="LIBGEO", freq = "POP", max.words = 2))

#Points
mtqcent=st_centroid(st_geometry(mtq[1,]), of_largest_polygon = TRUE)
mtqcent=st_sf(
  st_drop_geometry(mtq),
  row.names = 1:nrow(mtq),
  geometry=mtqcent
)
plot(st_geometry(mtq))
expect_silent(wordcloudLayer(mtqcent, txt ="LIBGEO", freq = "POP", cex=0.3, add=TRUE))
expect_error(wordcloudLayer(mtqcent, txt ="LIBGEO", freq = "POP", cex=0.3))

