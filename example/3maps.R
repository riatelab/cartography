library(cartography)

# Load data
data(nuts2006)



sizes <- getFigDim(spdf = nuts0.spdf, width = 400, mar = c(0,0,1.2,0))

sizes[1] <- sizes[1] * 3 


png(filename = "example/3maps.png", width = sizes[1], height = sizes[2], res = 150)

?png

# set margins
opar <- par(mar = c(0,0,1.2,0), mfrow = c(1,3))

# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)


plot(nuts0.spdf, col = "#D1914D",border = "white", lwd=0.5, add=TRUE)


nuts0.df$pop <- nuts0.df$pop2008 / 1000

# Plot the compound annual growth rate
propSymbolsLayer(spdf = nuts0.spdf, df = nuts0.df, var = "pop", 
                 inches = 0.15, lwd = 0.5,
                 legend.pos = "right", legend.values.rnd = 0, 
                 legend.title.txt = "Population, 2008\n(thousands of inh.)", 
                 legend.frame = F)
# Layout plot
layoutLayer(title = "\npropSymbolsLayer()",  author = "Package cartography v1.4.0", 
            sources = "Source: Eurostat, 2011", frame = FALSE,  
            scale = NULL, theme = "wine.pal",
            north = TRUE) # add a south arrow



nuts0.df$gdpinh <- nuts0.df$gdppps2008 * 1000000 / nuts0.df$pop2008

# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)

choroLayer(spdf = nuts0.spdf, df = nuts0.df, var = "gdpinh", 
           col = carto.pal(pal1 = "green.pal", n1 = 4), 
           method = "quantile",nclass = 4,  border = "white", lwd = 0.5,  
           legend.pos = "right", legend.title.txt = "GDP per inh. ,2008\n(euros)", 
           add=T)

layoutLayer(title = "\nchoroLayer()", author = "", 
            sources = "", frame = FALSE, 
            scale = NULL, theme = "green.pal",
            north = FALSE) 

# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
plot(nuts0.spdf, col = "#F8FD4050",border = "white", lwd=0.5, add=TRUE)
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
                      legend.var.title.txt = "Population, 2008\n(thousands of inh.)", # size legend title
                      legend.var.style = "e", # size legend type
                      legend.var2.pos = "right", # color legend position
                      legend.var2.values.rnd = 0,
                      legend.var2.title.txt = "GDP per inh., 2008\n(euros)") # legend title


# layout
layoutLayer(title = "\npropSymbolsChoroLayer()", author = "", 
            sources = "", frame = FALSE,  theme = "blue.pal",
            scale = 500, 
            north = FALSE) 




dev.off()

