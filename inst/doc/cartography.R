## ------------------------------------------------------------------------

data("nuts2006")
par(mar = c(0,0,1.2,0))
# Layout plot
layoutLayer(  title = "Most Populated Countries of Europe",
              author = "", sources = "",
              scale = NULL,col = NA, coltitle = "black",
              frame = FALSE, bg = "#A6CAE0",
              south = TRUE, extent = nuts0.spdf)

plot(world.spdf, col  = "#E3DEBF", border=NA, add=TRUE)
plot(nuts0.spdf, col = "#D1914D",border = "white", lwd=1, add=TRUE)

# Selection of the 10 most populated countries of Europe
dflab <- nuts0.df[order(nuts0.df$pop2008, decreasing = TRUE),][1:10,]

# Label creation
dflab$lab <- paste(dflab$id, "\n", round(dflab$pop2008/1000000,0), "M", sep ="")

# Label plot of the 10 most populated countries
labelLayer(spdf = nuts0.spdf, df = dflab, txt = "lab",
           col = "#690409", cex = 0.9, font = 2)
text(x = 5477360, y = 4177311, labels = "The 10 most populated countries of Europe
Total population 2008, in millions of inhabitants.",
     cex = 0.7, adj = 0)


