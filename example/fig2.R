library(cartography)
# Load data
data(nuts2006)
# set margins

# Create a grid layer
mygrid <- getGridLayer(spdf = nuts3.spdf, cellsize = 100000, type = "hexagonal")

# Compute data for the grid layer
df1 <- getGridData(x = mygrid, df = nuts3.df, var = "pop2008" )
df2 <- getGridData(x = mygrid, df = nuts3.df, var = "pop1999" )

# Compute the compound annual growth rate
df1$cagr <-(((df1$pop2008 / df2$pop1999)^(1/9)) - 1) * 100
v <- getBreaks(v = df1$cagr ,method = "quantile",nclass = 10)
v <- v[order(v)]
v[5] <- 0

cols <- carto.pal(pal1 = "pink.pal", n1 = 4,pal2 = "taupe.pal", n2 = 6, middle = F )


opar <- par(mar = c(0,0,1.2,0))
# Plot a layer with the extent of the EU28 countries with only a background color
plot(nuts0.spdf, border = NA, col = NA, bg = "#A6CAE0")

# Plot non european space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)

choroLayer(spdf = mygrid$spdf,df = df1,var = "cagr", add=TRUE,
           col=cols, lwd = 0.5, border = NA,
           legend.pos = "right", breaks = v,legend.values.rnd = 2,
           legend.title.txt = "COUMPOUND")


plot(nuts0.spdf, add=T, col = NA, border = "grey20", lwd = 1)


layoutLayer(title = "COUMPOUND", author = "cartography", 
            sources = "Eurostat, 2008", frame = TRUE, col = "white", 
            scale = 500, coltitle = "black",
            north = TRUE, theme = "taupe.pal") 


