#' @title Typology Layer
#' @name typoLayer
#' @description Plot a typology layer.
#' @param spdf a SpatialPolygonsDataFrame.
#' @param df a data frame that contains the values to plot. If df is missing 
#' spdf@data is used instead. 
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param dfid identifier field in df, default to the first column 
#' of df. (optional)
#' @param var name of the field in df to plot.
#' @param col a vector of colors.
#' @param border color of the polygons borders.
#' @param lwd borders width.
#' @param legend.pos position of the legend, one of "topleft", "top", 
#' "topright", "left", "right", "bottomleft", "bottom", "bottomright". If 
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.order values order in the legend, a character vector 
#' that matches var modalities. Colors will be affected following this order.  
#' @param legend.frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param legend.nodata no data label.
#' @param colNA no data color. 
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @seealso \link{propSymbolsTypoLayer}, \link{typoLayer}, \link{legendTypo}
#' @export
#' @examples
#' data(nuts2006)
#' ## Example 1
#' nuts0.df$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' typoLayer(spdf = nuts0.spdf, df = nuts0.df, var = "typo") 
#' 
#' 
#' ## Example 2
#' nuts0.df$typo <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",4))
#' typoLayer(spdf = nuts0.spdf, df = nuts0.df,
#'           var="typo",  col = carto.pal(pal1 = "multi.pal", 4),
#'           legend.values.order = c("D", "B", "A", "C"),
#'           legend.pos = "topright", 
#'           legend.title.txt = "Category")
#' layoutLayer(title = "Colors in Europe",
#'             sources = "UMS RIATE, 2015",
#'             scale = NULL,
#'             frame = TRUE,
#'             col = "black",
#'             coltitle = "white")
typoLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var, 
                      col = NULL, border = "grey20", lwd = 1,
                      colNA = "white",
                      legend.pos = "bottomleft", 
                      legend.title.txt = var,
                      legend.title.cex = 0.8, 
                      legend.values.cex = 0.6,
                      legend.values.order = NULL,
                      legend.nodata = "no data",
                      legend.frame = FALSE,
                      add = FALSE)
{
  # Check missing df and NULL identifiers 
  if (missing(df)){df <- spdf@data}
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid <- names(df)[1]}
  
  # Join
  spdf@data <- data.frame(spdf@data[,spdfid], 
                          df[match(spdf@data[,spdfid], df[,dfid]),])
  
  # modalities
  mod <- unique(spdf@data[, var])
  mod <- mod[!is.na(mod)]
  # check nb col vs nb mod
  col <- checkCol(col, mod)
  # check legend.values.order vs mod values
  legend.values.order <- checkOrder(legend.values.order, mod)
  # get the colors 
  refcol <- data.frame(mod = legend.values.order, 
                       col = col[1:length(legend.values.order)], 
                       stringsAsFactors = FALSE)
  colvec <- refcol[match(spdf@data[,var], refcol[,1]),2]

  # for the legend  
  mycols <- refcol[,2]
  rVal <- refcol[,1]
  
  # for NA values
  nodata <- FALSE
  if(max(is.na(df[,var]) > 0)){
    nodata <- TRUE
    colvec[is.na(colvec)] <- colNA
  }

  # plot
  plot(spdf, col = colvec, border = border, lwd = lwd, add = add)
  
  

  
  if(legend.pos !="n"){
    legendTypo(pos = legend.pos, title.txt = legend.title.txt,
               title.cex = legend.title.cex, values.cex = legend.values.cex,
               categ = rVal, 
               col = mycols, 
               frame = legend.frame, 
               symbol="box", 
               nodata = nodata,
               nodata.col = colNA,
               nodata.txt = legend.nodata)
  }
}
