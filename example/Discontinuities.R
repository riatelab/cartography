library(cartography)
# Load data
data(nuts2006)
# set margins
opar <- par(mar = c(0,0,1.2,0))

# Get a SpatialLinesDataFrame of countries borders
nuts2.contig <- getBorders(nuts2.spdf)

nuts2.df$gdpcap <- nuts2.df$gdppps2008 / nuts2.df$pop2008 * 1000000



sizes <- getFigDim(spdf = nuts0.spdf, width = 1200, mar = c(0,0,1.2,0))

png(filename = "example/Discontinuities.png", width = sizes[1], height = sizes[2],res = 150)

# set margins
opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,1))



# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts2.spdf, border = NA, col = NA, bg = "#A6CAE0")
# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)

# Plot GDP per capita with a choropleth layer
choroLayer(spdf = nuts2.spdf, df = nuts2.df, var = "gdpcap", border = "grey20", lwd = 0.2,
           col = carto.pal(pal1 = "green.pal", n1 = 3, "sand.pal", 3 ), method = "quantile",
           nclass = 6, add=TRUE, legend.pos = "right", 
           legend.values.rnd = -2,
           legend.title.txt = "GDP per Capita\n(in euros)")


# Plot discontinuities
discLayer(spdf = nuts2.contig, # SpatialLinesDataFrame of borders
          df = nuts2.df, # data frame on countries
          var = "gdpcap", # variable used to compute discontinuties 
          type = "rel", # type of discontinuity measure 
          method="equal", # discretisation of discontinuities
          nclass = 3, # number of discontinuities classes
          threshold = 0.4, # representation threshold of discontinuities  
          sizemin = 0.7, # minimum size of discontinuities lines
          sizemax = 6, # maximum size of discontinuities lines
          col="red", # color of the lines
          legend.values.rnd = 1,
          legend.title.txt = "Discontinuities in \nGDP per Capita\n(relative)",
          legend.pos = "topright", 
          add=TRUE)

# Layout
layoutLayer(title = "Wealth Disparities in Europe, 2008",author = "Package cartography v1.4.0", 
            sources = "Source: Eurostat, 2011", frame = TRUE, 
            scale = 500,  theme = "grey.pal")
dev.off()

