#' @title chinesehatsLayer
#' @description Plot a "chinese hat" layer
#' @name chinesehatsLayer
#' @param spdf SpatialPolygonsDataFrame
#' @param df DataFrame with Ids and Labels
#' @param spdfid Ids of the obj Spatial*DataFrame
#' @param dfid Ids of the DataFrame
#' @param var1 Symbols variable 1 (numeric)
#' @param var2 Symbols variable 2 (numeric)
#' @param col1 Symbols color top
#' @param col2 Symbols color bottom
#' @param k Share of the map occupied by symbols
#' @param add Whether to add the layer to an existing map (TRUE) or not (FALSE)
#' @export
#' @import sp
#' @examples
#' data("nuts2006")
#' nuts0.df$sharepop <- round((nuts0.df$pop2008 / sum(nuts0.df$pop2008))*100,2)
#' nuts0.df$sharegdp <- round((nuts0.df$gdppps2008 / sum(nuts0.df$gdppps2008))*100,2)
#' plot(countries.spdf,col="#E0E0E0",border="white",lwd=1)
#' plot(nuts0.spdf,col="#E5CFC1",border="white",lwd=2,add=TRUE)
#' chinesehatsLayer(spdf = nuts0.spdf, df = nuts0.df, spdfid=NULL, dfid=NULL,var1 = "sharepop",
#' var2 = "sharegdp", col1="#FF9100",col2="#45C945",k = 0.2, add=TRUE)
#' @export

chinesehatsLayer <- function(spdf, df, spdfid=NULL, dfid=NULL,var1, var2, col1="red",col2="blue",k, add=TRUE){


if (is.null(spdfid)){spdfid<-names(spdf@data)[1]}
if (is.null(dfid)){dfid<-names(df)[1]}

dots <- cbind(spdf@data[,spdfid],as.data.frame(coordinates(spdf)))
colnames(dots) <- c(spdfid,"x","y")
dots <- data.frame(dots, df[match(dots[,spdfid], df[,dfid]),])
dots <- dots[,c(spdfid,"x","y",var1,var2)]

# Ajustage de la atille des cercles
x1 <- sp::bbox(spdf)[1]
y1 <- sp::bbox(spdf)[2]
x2 <- sp::bbox(spdf)[3]
y2 <- sp::bbox(spdf)[4]
if (sum(df[,var1])>=sum(df[,var2])){var <- var1} else {var <- var2}
sfdc <- (x2-x1)*(y2-y1)
sc <- sum(abs(dots[,var]),na.rm = TRUE)


if(add==FALSE){plot(spdf)}

# TRIANGLE TOP
dots$size1 <-  sqrt(dots[,var1]*k* sfdc / sc /2)
dots$y1<-dots$y+dots$size1/2
dots <- dots[order(dots[,var1],decreasing=TRUE),]
dots$xx1a<-dots$x-dots$size1/2
dots$xx1b<-dots$x
dots$xx1c<-dots$x+dots$size1/2
dots$yy1a<-dots$y
dots$yy1b<-dots$y+dots$size1/2
dots$yy1c<-dots$y
for (i in 1:length(dots$x)){polygon(c(dots$xx1a[i],dots$xx1b[i],dots$xx1c[i]), c(dots$yy1a[i],dots$yy1b[i],dots$yy1c[i]), col = col1, border = "#DDDDDD")}

# TRIANGLE BOTTOM
dots$size2 <-  sqrt(dots[,var2]*k* sfdc / sc /2)
dots$y1<-dots$y+dots$size2/2
dots <- dots[order(dots[,var2],decreasing=TRUE),]
dots$xx1a<-dots$x-dots$size2/2
dots$xx1b<-dots$x
dots$xx1c<-dots$x+dots$size2/2
dots$yy1a<-dots$y
dots$yy1b<-dots$y-dots$size2/2
dots$yy1c<-dots$y
for (i in 1:length(dots$x)){polygon(c(dots$xx1a[i],dots$xx1b[i],dots$xx1c[i]), c(dots$yy1a[i],dots$yy1b[i],dots$yy1c[i]), col = col2, border = "#DDDDDD")}

}


