library(cartography)
# Load data
data(nuts2006)

# Create a grid layer
mygrid <- getGridLayer(spdf = nuts3.spdf, cellsize = 50000, type = "regular")
# # saveRDS(mygrid, 'example/mygrid.rds')
# mygrid <- readRDS('example/mygrid.rds')


nuts3.df$gdp <- nuts3.df$gdppps2008 * 1000000
# Compute data for the grid layer
df1 <- getGridData(x = mygrid, df = nuts3.df, var = "pop2008" )
df2 <- getGridData(x = mygrid, df = nuts3.df, var = "gdp" )
df1$gdp <- df2$gdp


v <- c(2920, 5000, 10000, 15000, 20000, 23500, 30000, 35000, 40000, 42720)

y <- smoothLayer(spdf = mygrid$spdf, df = df1, var = "gdp", var2 = "pop2008", breaks = v,  
                 span = 100000, beta = 2, mask = nuts0.spdf, resolution = 49000)


sizes <- getFigDim(spdf = nuts0.spdf, width = 1200, mar = c(0,0,1.2,0))

png(filename = "example/IsoplethMap.png", width = sizes[1], height = sizes[2],res = 150)

opar <- par(mar = c(0,0,1.2,0))

# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
cols <- c(rev(carto.pal("green.pal", 5)), carto.pal("orange.pal", 4))
choroLayer(spdf = y, var = "center", col=cols, breaks = v, 
           legend.title.txt = "Potential\nGDP per capita\n(in euros)", legend.values.rnd = -2,
           border = "grey80", lwd = 0.5, add=T, legend.pos = "topright")


plot(rgeos::gBuffer(nuts0.spdf,FALSE , 1), add=T, col = NA, border = "grey50")


layoutLayer(title = "Wealth Inequalities in Europe, 2008", 
            author = "Package cartography v1.4.0", 
            sources = "Source: Eurostat, 2011", frame = TRUE, 
            scale = 500, 
            north = FALSE, theme = "sand.pal") 

text(x = 6271272, y = 3743765, 
     labels = "Distance function:\n- type = exponential\n- beta = 2\n- span = 100 km", 
     cex = 0.8, adj = 0, font = 3)

dev.off()