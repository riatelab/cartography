#' @title Spatial discontinuities
#' @description Compute and plot spatial discontinuities
#' @name discLayer
#' @details Compute and plot spatial discontinuities (relative or absolutes)
#' @param spdf SpatialLinesDataFrame (geometric borders)
#'
#'
#' @param df dataframe (data)
#' @param spdfid1 first id of the border
#' @param spdfid2 second id of the border
#' @param dfid id of the df (if null, the first column is used)
#' @param var field used
#' @param distr vector of classes
#' @param col Color (character)
#' @param nbclass number of classes targeted (if null,
#' the Huntsberger method is used)
#' @param method discretization method ("sd", "equal",
#' "quantile", "jenks","q6","geom")
#' @param threshold Value between 0 (nothing) and 1 (all the discontinuities). Default = 0.75
#' @param sizemin thickness of the smaller line (default = 1)
#' @param sizemax thickness of the bigger line (default = 10)
#' @param type Mesuer of discontinuity. "rel","abs" (default = "rel")
#' @param add add=TRUE
#'
#' @examples
#' data(nuts2006)
#' nuts0.contig.spdf <-borders(nuts0.spdf)
#' plot(nuts0.spdf, col="#CCCCCC", lwd=1, border="white")
#' discLayer(spdf = nuts0.contig.spdf, df = nuts0.df, dfid = "id", spdfid1 = "id1", spdfid2 = "id2",
#' var = "gdppps2008", col="red", nbclass=5,  method="quantile", threshold = 0.5, sizemin = 1,
#' sizemax = 10, type = "rel", add=TRUE )
#'
#' @return plot
#' @export


discLayer <- function(spdf, df, spdfid1 = NULL, spdfid2=NULL, dfid=NULL,
                      var, distr = NULL, col= "red", nbclass=NULL,
                      method="quantile", threshold = 0.75, sizemin = 1,
                      sizemax = 10, type = "rel",add = FALSE){


# Join (1 and 2)
spdf@data <- data.frame(spdf@data, var1=df[match(spdf@data[,spdfid1], df[,dfid]),var])
spdf@data <- data.frame(spdf@data, var2=df[match(spdf@data[,spdfid2], df[,dfid]),var])

# discontinuité relative ou absolue
if (type == "rel") {spdf@data$disc <- pmax(spdf@data$var1/spdf@data$var2,spdf@data$var2/spdf@data$var1)}
if (type == "abs") {spdf@data$disc <- pmax(spdf@data$var1-spdf@data$var2,spdf@data$var2-spdf@data$var1)}

# Valeur muinimal
  minvar<-as.numeric(quantile(spdf@data$disc,probs = c(threshold)))

# Discretisation
spdf <- spdf[spdf@data$disc >=minvar,]
distr <- discretization(v=spdf@data$disc,nbclass=nbclass,method=method)

# Classes de tailles
x <- (sizemax-sizemin)/nbclass
sizes <- sizemin
for(i in 1:nbclass){sizes <- c(sizes,sizes[i]+x)}

# Affectation des tailles au spdf
sizesMap <- sizes[(findInterval(spdf@data$disc,distr,all.inside=TRUE))]
spdf@data <- data.frame(spdf@data,sizesMap)

# Cartographie
plot(spdf, col=col,lwd=spdf@data$sizesMap,add=add)

# Legend

LegendSizeLines(pos = "bottomleft", legTitle = "Title of the legend", legTitleCex = 0.8,
legValuesCex = 0.6, distr = distr, thickness = sizes, col =col, round =2, nodata = FALSE)


}

