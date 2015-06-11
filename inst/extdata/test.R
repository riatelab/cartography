library(flow)


mat <- MRE44[,c(3,1,5)]
mat <- prepflows(mat = MRE44, i = "DCRAN", j = "CODGEO", fij = "NBFLUX_C08_POP05P")
diag(mat) <- 0
matsel <- firstflows(mat, method = 'nfirst', k = 1)
xx <- reshape2::melt(matsel * mat)
input<- data.frame(id = names(colSums(matsel*mat)),var = colSums(matsel*mat))
input<- input[input$var>0,]
xx$col <- "red"
xx$prop <- runif(nrow(xx),-10,10)

LA <- getOSMLayer(spdf = COM44, type = "stamen-toner")
osmLayer(LA)
staticLayer(COM44, col = NA, add=T, border = "grey50")
layoutLayer("Loire Atlantik Residential Flows", col = "#616628", coltitle = "white", north = T)
# propLinkLayer(spdf = COM44, df = xx, spdfid = NULL, dfidi = NULL, dfidj = NULL,
#               var1 = NULL, var2 = "col", maxlwd = 20)
propLinkChoroLayer(spdf = COM44, df = xx, maxlwd = 50, var2 = "prop",
                   nbclass = 10, method = "equal",
                   col = carto.pal("kaki.pal", 5, pal2 = "blue.pal", 5, middle = F))
propSymbolsLayer(spdf = COM44, df = input, var = "var", k = 0.01, col = "#939AC950")
dev.off()
# propLinkLayer : Affiche des liens de couleurs et de taille dépendant de 2 variables
# propLinkChoroLayer : Affiche des liens dont la couleur dépend d'une discretisation d'une variable

OpenStreetMap::openmap

propLinkLayer <- function(spdf, df, spdfid = NULL, dfidi = NULL, dfidj = NULL,
                          maxlwd = 40, var1 = NULL, var2 = NULL){
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfidi)){dfidi <- names(df)[1]}
  if (is.null(dfidj)){dfidj <- names(df)[2]}
  if (is.null(var1)){var1 <- names(df)[3]}

  geomOri <- data.frame (coordinates(spdf), spdf@data[,spdfid])
  names(geomOri) <- c("xOri", "yOri", "idOri")
  geomDes <- data.frame (coordinates(spdf), spdf@data[,spdfid])
  names(geomDes) <- c("xDes", "yDes", "idDes")

  df <- df[df[,var1] > 0,]
  flux <- merge(df, geomOri, by.x = dfidi, by.y = "idOri", all.x = T)
  flux <- merge(flux, geomDes, by.x =  dfidj, by.y = "idDes", all.x = T)

  flux$lwd <- flux[,var1] / (max(flux[,var1]) / maxlwd)

  if(!is.null(var2)){flux$col <- flux[,var2]}else{flux$col = "grey10"}
  segments(x0 = flux[,"xOri"], y0 = flux[,"yOri"],
           x1 = flux[,"xDes"], y1 = flux[,"yDes"],
           col = flux$col,
           lwd = flux$lwd)
}




propLinkChoroLayer <- function(spdf, df, spdfid = NULL, dfidi = NULL, dfidj = NULL,
                               var1 = NULL, var2 = NULL, distr = NULL,
                               col = carto.pal("kaki.pal", n1 = 6), nbclass = 6,
                               method = "quantile", maxlwd = 20){

  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfidi)){dfidi <- names(df)[1]}
  if (is.null(dfidj)){dfidj <- names(df)[2]}
  if (is.null(var1)){var1 <- names(df)[3]}
  if (is.null(var2)){var2 <- names(df)[3]}

  geomOri <- data.frame (coordinates(spdf), spdf@data[,spdfid])
  names(geomOri) <- c("xOri", "yOri", "idOri")
  geomDes <- data.frame (coordinates(spdf), spdf@data[,spdfid])
  names(geomDes) <- c("xDes", "yDes", "idDes")

  df <- df[df[,var1] > 0,]
  flux <- merge(df, geomOri, by.x = dfidi, by.y = "idOri", all.x = T)
  flux <- merge(flux, geomDes, by.x =  dfidj, by.y = "idDes", all.x = T)

  flux$lwd <- flux[,var] / (max(flux[,var1]) / maxlwd)
  chorococo <- choro(var=flux[,var2], distr = distr,
                     col = col,
                     nbclass = nbclass,
                     method = method)

  flux$col <- chorococo$colMap
  segments(x0 = flux[,"xOri"], y0 = flux[,"yOri"],
           x1 = flux[,"xDes"], y1 = flux[,"yDes"],
           col = flux$col, lwd = flux$lwd)
  print(chorococo$col)
  print(chorococo$distr)
}





