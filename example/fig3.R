library(cartography)
# Load data
data(nuts2006)

# Create a grid layer
mygrid <- getGridLayer(spdf = nuts3.spdf, cellsize = 50000, type = "regular")
saveRDS(mygrid, 'example/mygrid.rds')

plot(mygrid$spdf)

nuts3.df$gdp <- nuts3.df$gdppps2008
# Compute data for the grid layer
df1 <- getGridData(x = mygrid, df = nuts3.df, var = "pop2008" )
df2 <- getGridData(x = mygrid, df = nuts3.df, var = "gdp" )
df1$gdp <- df2$gdp

v <- c(seq(2800, 23500, 2000), seq(23500,59000, 4000))
y <- smoothLayer(spdf = mygrid$spdf, df = df1, var = "gdp", var2 = "pop2008",breaks = v, 
                 span = 50000, beta = 2, mask = nuts0.spdf, resolution = 49000)

cols <- c(rev(carto.pal("green.pal", 11)), carto.pal("wine.pal", 8))

opar <- par(mar = c(0,0,1.2,0))
# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)

choroLayer(spdf = y, var = "center", breaks = bks, col=cols,
           border = "grey80", lwd = 0.5, add=T, legend.pos = "topright")

plot(nuts0.spdf, border = "grey20", col = NA, bg = NA, add=T)

layoutLayer(title = "Smooth", author = "cartography", 
            sources = "Eurostat, 2008", frame = TRUE, col = "white", 
            scale = 500, coltitle = "black",
            north = TRUE, theme = "wine.pal") 

