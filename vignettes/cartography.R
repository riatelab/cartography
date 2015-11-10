## ----labelMap, fig.height=5, fig.width=7---------------------------------
library(cartography)
# Load data
data(nuts2006)
# set margins
opar <- par(mar = c(0,0,1.2,0))

# Layout plot
layoutLayer(title = "Most Populated Countries of Europe", # title of the map
            author = "",  # no author text
            sources = "", # no source text
            scale = NULL, # no scale
            col = NA, # no color for the title box 
            coltitle = "black", # color of the title
            frame = FALSE,  # no frame around the map
            bg = "#A6CAE0", # background of the map
            extent = nuts0.spdf) # set the extent of the map

# Non European space
plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
# European (EU28) countries
plot(nuts0.spdf, col = "#D1914D",border = "white", lwd=1, add=TRUE)

# Selection of the 10 most populated countries of Europe
dflab <- nuts0.df[order(nuts0.df$pop2008, decreasing = TRUE),][1:10,]
# Label creation
dflab$lab <- paste(dflab$id, "\n", round(dflab$pop2008/1000000,0), "M", sep ="")

# Label plot of the 10 most populated countries
labelLayer(spdf = nuts0.spdf, # SpatialPolygonsDataFrame used to plot he labels
           df = dflab, # data frame containing the lables
           txt = "lab", # label field in df
           col = "#690409", # color of the labels
           cex = 0.9, # size of the labels
           font = 2) # label font

# Add an explanation text
text(x = 5477360, y = 4177311, labels = "The 10 most populated countries of Europe
Total population 2008, in millions of inhabitants.", cex = 0.7, adj = 0)

par(opar)

