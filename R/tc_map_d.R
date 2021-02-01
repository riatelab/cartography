#' @title Plot a discontinuities map
#' @description This function computes and plots spatial discontinuities. The 
#' discontinuities are plotted over a border layer (see \link{tc_get_borders}.
#' The line widths reflect the ratio or the difference between values of an indicator 
#' in two neighbouring units.
#' @name tc_map_disc
#' @param x an sf object, a simple feature collection, as outputted by the \link{tc_get_borders} function. 
#' @param df a data.frame that contains the values used to compute and plot discontinuities.
#' @param df_id name of the identifier variable in df, default to the first column 
#' of df. (optional)
#' @param var name of the numeric variable used to compute and plot discontinuities.
#' @param col color of the discontinuities lines.
#' @param nbreaks a targeted number of classes. If null, the number of 
#' class is automatically defined (see \link{tc_get_breaks}).
#' @param breaks a classification method; one of "sd", "equal", "quantile", "fisher-jenks","
#' q6", "geom", "arith", "em" or "msd" (see \link{getBreaks}).
#' @param threshold share of represented borders, value between 0 
#' (nothing) and 1 (all the discontinuities).
#' @param sizemin thickness of the smallest line.
#' @param sizemax thickness of the biggest line.
#' @param type type of discontinuity measure, one of "rel" or "abs" (see Details).
#' @param leg_pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", "left" or a 
#' vector of two coordinates in map units (c(x, y)). If 
#' legend.pos is "n" then the legend is not plotted.
#' @param leg_title title of the legend.
#' @param leg_title_cex size of the legend title.
#' @param leg_val_cex size of the values in the legend.
#' @param leg_val_rnd number of decimal places of the values in 
#' the legend.
#' @param leg_frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param add whether to add the layer to an existing plot (TRUE) or 
#' not (FALSE).
#' @return  An \link{invisible} sf object (MULTISTRING) with the discontinuity measures is returned. 
#' @details 
#' The "rel" type of discontinuity is the result of pmax(value unit 1 / value unit 2, value unit 2 / value unit 1).\cr
#' The "abs" type of discontinuity is the result of pmax(value unit 1 - value unit 2, value unit 2 - value unit 1).
#' @examples
#' mtq <- tc_import_mtq()
#' # Get borders
#' mtq_borders <- tc_get_borders(x = mtq)
#' # Median Income
#' tc_map_c(x = mtq, var = "MED", border = "grey", lwd = 0.5,
#'          breaks = 'equal', nbreaks = 6, leg_pos = "topleft",
#'          leg_title = "Median Income\n(in euros)" )
#' # Discontinuities
#' tc_map_disc(x = mtq_borders, df = mtq, 
#'             var = "MED", col = "red4", nbreaks=3,
#'             breaks="equal", threshold = 0.5, sizemin = 0.5,
#'             sizemax = 10, type = "abs",leg_val_rnd = 0,
#'             leg_title = "Discontinuities\n(absolute difference)",
#'             leg_pos = "bottomleft", add = TRUE)
#' @export
tc_map_disc <- function(x, 
                        df, 
                        df_id, 
                        var, 
                        breaks = "quantile", 
                        nbreaks = 4, 
                        threshold = 0.75, 
                        type = "rel",
                        sizemin = 1, 
                        sizemax = 10,
                        col = "tomato4", 
                        leg_pos = "bottomleft",
                        leg_title="legend title",  
                        leg_title_cex = 0.8, 
                        leg_val_cex = 0.6, 
                        leg_val_rnd = 2,
                        leg_frame=FALSE,
                        add = TRUE){
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  bg <- .gmapsf$args$bg
  fg <- .gmapsf$args$fg
  
  if (missing(df_id)){df_id <- names(df)[1]}
  df <- data.frame(df)
  # Join (1 and 2)
  x <- merge(x, df[,c(df_id, var)], by.x = names(x)[3], by.y = df_id, 
             all.x = TRUE)
  x <- merge(x, df[,c(df_id, var)], by.x = names(x)[3], by.y = df_id, 
             all.x = TRUE)
  names(x)[4:5] <- c('var1', 'var2')
  
  # elimination des valeurs manquantes
  x <- x[!is.na(x$var1) & !is.na(x$var2), ]
  
  # discontinuité relative ou absolue
  if (type == "rel") {x$disc <- pmax(x$var1/x$var2, x$var2/x$var1)}
  if (type == "abs") {x$disc <- pmax(x$var1-x$var2, x$var2-x$var1)}
  
  x.out <- x
  colnames(x.out)[4:5] <- c(paste(var, 1, sep=""), paste(var, 2, sep=""))
  
  # Valeur minimal
  minvar <- as.numeric(quantile(x$disc,probs = c(1-threshold)))
  
  # Discretisation
  x <- x[x$disc >= minvar,]
  distr <- tc_get_breaks(x = x$disc, nbreaks = nbreaks, breaks = breaks)
  
  # Classes de tailles
  # ete <- (sizemax - sizemin) / nbreaks-1
  # sizes <- sizemin
  # for(i in 1:nbreaks){sizes <- c(sizes,sizes[i] + ete)}
  
  sizes <- seq(sizemin, sizemax, length.out = nbreaks)
  
  
  # Affectation des tailles au spdf
  x$sizesMap <- sizes[(findInterval(x$disc,distr,all.inside=FALSE, rightmost.closed = TRUE))]
  
  # Cartographie
  plot(sf::st_geometry(x), col = col, lwd = x$sizesMap, add = add, bg = bg)
  
  print(unique(x$sizesMap))
  # Legend
  tc_leg_gl(pos = leg_pos, 
            val = distr, 
            lwd = sizes,
            title = leg_title,
            title_cex = leg_title_cex ,val_cex = leg_val_cex,
            val_rnd =  leg_val_rnd, col = col, 
            bg = bg, fg = fg, frame = leg_frame)
  
  invisible(x.out)
}
