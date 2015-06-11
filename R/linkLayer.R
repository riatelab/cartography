#' @name propLinkLayer
#' @title Layer of Proportional Links
#' @description Plot a layer of links
#' @param spdf Spatial*DataFrame
#' @param df DataFrame with Ids and Labels
#' @param spdfid Ids of the obj Spatial*DataFrame
#' @param dfidi Ids of origins
#' @param dfidj Ids of destinations
#' @param var1 name of the variable corresponding to lwd of the links
#' @param var2 name of the variable corresponding to colors
#' @param maxlwd max lwd of the links
#' @return a plot
#' @export
propLinkLayer <- function(spdf, df, spdfid = NULL, dfidi = NULL, dfidj = NULL,
                          var1 = NULL, var2 = NULL, maxlwd = 40){
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfidi)){dfidi <- names(df)[1]}
  if (is.null(dfidj)){dfidj <- names(df)[2]}
  if (is.null(var1)){var1 <- names(df)[3]}

  geomOri <- data.frame (sp::coordinates(spdf), spdf@data[,spdfid])
  names(geomOri) <- c("xOri", "yOri", "idOri")
  geomDes <- data.frame (sp::coordinates(spdf), spdf@data[,spdfid])
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

#' @name propLinkChoroLayer
#' @title Layer of Proportional Links Colored with a Variable
#' @description Plot a layer of links colors from a discretization
#' @param spdf Spatial*DataFrame
#' @param df DataFrame with Ids and Labels
#' @param spdfid Ids of the obj Spatial*DataFrame
#' @param dfidi Ids of origins
#' @param dfidj Ids of destinations
#' @param var1 name of the variable corresponding to lwd of the links
#' @param var2 name of the variable corresponding to discretize
#' @param distr vector of classes
#' @param col vector of colors
#' @param nbclass number of classes targeted (if null,
#' the Huntsberger method is used)
#' @param method discretization method ("sd", "equal",
#' "quantile", "jenks","q6","geom")
#' @param maxlwd max lwd of the links
#' @return a plot
#' @export
propLinkChoroLayer <- function(spdf, df, spdfid = NULL, dfidi = NULL,
                               dfidj = NULL, var1 = NULL, var2 = NULL,
                               distr = NULL,
                               col = carto.pal("kaki.pal", n1 = 6),
                               nbclass = 6,
                               method = "quantile",
                               maxlwd = 20){
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
  choroout <- choro(var=flux[,var2], distr = distr,
                    col = col,
                    nbclass = nbclass,
                    method = method)

  flux$col <- choroout$colMap
  segments(x0 = flux[,"xOri"], y0 = flux[,"yOri"],
           x1 = flux[,"xDes"], y1 = flux[,"yDes"],
           col = flux$col, lwd = flux$lwd)
  print(choroout$col)
  print(choroout$distr)
}
