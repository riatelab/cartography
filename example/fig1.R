library(cartography)

# Load data
data(nuts2006)

# set margins
opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,3))

# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)


plot(nuts0.spdf, col = "#D1914D",border = "white", lwd=1, add=TRUE)


nuts0.df$pop <- nuts0.df$pop2008 / 1000

# Plot the compound annual growth rate
propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, var = "pop", inches = 0.15, 
                 legend.pos = "right", legend.values.rnd = 0, 
                 legend.title.txt = "Population\n(thousands of inh.)", 
                 legend.frame = F)
# Layout plot
layoutLayer(title = "propSymbolsLayer()", author = "cartography", 
            sources = "Eurostat, 2008", frame = TRUE, col = "white", 
            scale = 500, coltitle = "black",
            north = TRUE) # add a south arrow

nuts0.df$gdpinh <- nuts0.df$gdppps2008 * 1000000 / nuts0.df$pop2008

# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)

choroLayer(spdf = nuts0.spdf, df = nuts0.df, var = "gdpinh", 
           col = carto.pal(pal1 = "green.pal", n1 = 4), 
           method = "quantile",nclass = 4,  border = "white", lwd = 0.5,  
           legend.pos = "right", legend.title.txt = "GDP per inh.\n(euros)", 
           add=T)

layoutLayer(title = "choroLayer()", author = "cartography", 
            sources = "Eurostat, 2008", frame = TRUE, col = "white", 
            scale = 500, coltitle = "black",
            north = TRUE) 

# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
plot(nuts0.spdf, col = "#D1914D",border = "white", lwd=1, add=TRUE)
# Plot symbols with choropleth coloration
propSymbolsChoroLayer(spdf = nuts0.spdf, 
                      df = nuts0.df, 
                      var = "pop", #  field in df to plot the symbols sizes
                      inches = 0.2, # set the symbols sizes
                      var2 = "gdpinh", #  field in df to plot the colors
                      col = carto.pal(pal1 = "blue.pal", n1 = 4),symbols = "square",
                      method = "quantile", 
                      nclass = 4, 
                      border = "grey50",  # border colors of the symbols
                      lwd = 0.5, # symbols width
                      legend.var.pos = "topright", # size legend position
                      legend.var.values.rnd = 0, # size legend value roundinf
                      legend.var.title.txt = "Population\n(thousands of inh.)", # size legend title
                      legend.var.style = "e", # size legend type
                      legend.var2.pos = "right", # color legend position
                      legend.var2.values.rnd = 0,
                      legend.var2.title.txt = "GDP per inh.\n(euros)") # legend title


# layout
layoutLayer(title = "propSymbolsChoroLayer()", author = "cartography", 
            sources = "Eurostat, 2008", frame = TRUE, col = "white", 
            scale = 500, coltitle = "black",
            north = TRUE) 




# dev.off()

