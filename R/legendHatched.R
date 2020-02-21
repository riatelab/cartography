#' @title Legend for Hatched Maps
#' @description Plot legend for hatched maps.
#' @name legendHatched
#' @param pos position of the legend, one of "topleft", "top", 
#' "topright", "right", "bottomright", "bottom", "bottomleft", 
#' "bottomleftextra", "left" or a vector of two coordinates in map units 
#' (c(x, y)). 
#' @param title.txt title of the legend.
#' @param title.cex size of the legend title.
#' @param values.cex size of the values in the legend.
#' @param categ vector of categories.
#' @param patterns vector of patterns to be created for each element on \code{categ}, see \link{hatchedLayer}.
#' @param ptrn.bg background of the legend box for each \code{categ}.
#' @param ptrn.text text to be used for each \code{categ="text"}, as a single value or a vector.
#' @param dot.cex cex of each \code{patterns = "dot"} categories, as a single value or a vector.
#' @param text.cex text size of each \code{patterns = "text"} categories, as a single value or a vector.
#' @param cex size of the legend. 2 means two times bigger.
#' @param frame whether to add a frame to the legend (TRUE) or 
#' not (FALSE).
#' @param ... optional graphical parameters, see details on \link{hatchedLayer}
#' @seealso \link{hatchedLayer}, \link{legendTypo}
#' @author dieghernan, \url{https://github.com/dieghernan/}
#' @note It is also possible to create solid legends, by setting \code{col} and \code{ptrn.bg} to the same color. 
#' Parameters would honour the order of the \code{categ} variable.
#' @export
#' @examples 
#' library(sf)
#' mtq <-  st_read(system.file("gpkg/mtq.gpkg",
#'                             package = "cartography"),
#'                 stringsAsFactors = FALSE)
#' 
#' breaksMED<-getBreaks(mtq$MED, nclass=3)
#' ord=unique(mtq$STATUS)
#' ord<-ord[c(2,3,1)]
#' typoLayer(mtq,  var = "STATUS",  legend.pos = "n",
#'           legend.values.order = ord,
#'           col = c("grey10", "grey50", "grey80"),border = NA)
#' 
#' mtq$Patts = cut(mtq$MED,c(-Inf,14000,18000,Inf), labels=FALSE)
#' hatchedLayer( mtq[mtq$Patts == 1,],"zigzag",
#'               density = 2,  col = "white",  add = TRUE,  pch = 3,  cex = 0.6)
#' hatchedLayer(mtq[mtq$Patts == 2, ],"horizontal",
#'              density = 4, col = "white", add = TRUE)
#' hatchedLayer(mtq[mtq$Patts == 3, ], "left2right", col = "white",
#'              add = TRUE, density = 1.5)
#' 
#' legendHatched(pos = "bottomleft",
#'               cex = 1.5,
#'               values.cex = 0.8,
#'               title.txt = "Median Income (€)",
#'               categ = c("< 14k €","14k-18k €",">18k €", "Prefecture", "Sub-prefecture","Simple municipality"),
#'               patterns = c("zigzag","horizontal","left2right"),
#'               col = c(rep("black", 3), "grey10", "grey50", "grey80"),
#'               ptrn.bg = c(rep("white", 3), "grey10", "grey50", "grey80"),
#'               pch = 3
#' )
#' plot(st_geometry(st_union(mtq)), add = TRUE)
legendHatched <- function(pos = "topleft",
                          title.txt = "Title of the legend",
                          title.cex = 0.8,
                          values.cex = 0.6,
                          categ,
                          patterns,
                          ptrn.bg = "white",
                          ptrn.text = "X",
                          dot.cex = 0.5,
                          text.cex = 0.5,
                          cex = 1,
                          frame = FALSE,
                          ...) {
  # Basic controls #
  todot <- c("dot", "text")
  tolines <- c("diamond","grid","hexagon",
              "horizontal", "vertical","zigzag",
              "left2right","right2left","circle")
  

  
  # Store defaults #
  # Goal is to create a df with all the graphical params to be applied
  dots <- list(...) #additional params
  ncat <- length(categ)
  params <- data.frame(categ = categ,
                      stringsAsFactors = F
  )
  params$pattern <- rep(patterns, ncat)[1:ncat]
  params$legendfill <- rep(ptrn.bg, ncat)[1:ncat]
  col <- ifelse(rep(is.null(dots$col), ncat),
               par()$col,
               dots$col)
  
  params$col <- col
  density <- ifelse(rep(is.null(dots$density), ncat),
               1,
               dots$density)
  
  params$density <- density
  rm(patterns, ptrn.bg, density)
  params[,c(1,2)]
  
  # params forLines
  nlines <- nrow(params[params$pattern %in% tolines,])
  ltydef <- ifelse(is.null(dots$lty), par()$lty, NA)
  if (!is.na(ltydef)) {
    ltytext <- c("blank","solid",
                "dashed","dotted",
                "dotdash","longdash",
                "twodash")
    ltytopar <- match(ltydef, ltytext) - 1
    ltytopar <- rep(ltytopar, nlines)[1:nlines]
  } else {
    ltytopar <- rep(dots$lty, nlines)[1:nlines]
  }
  auxlist <- rep(NA, ncat)
  auxlist[params$pattern %in% tolines] <- ltytopar
  params$line.lty <- auxlist
  lwd <- ifelse(rep(is.null(dots$lwd), nlines),
               par()$lwd, dots$lwd
  )
  auxlist[params$pattern %in% tolines] <- lwd
  params$line.lwd <- auxlist
  rm(lwd, nlines)
  
  # params for Dots
  ndots <- nrow(params[params$pattern == "dot",])
  pch <- ifelse(rep(is.null(dots$pch), ndots),
               par()$pch,
               dots$pch
  )
  auxlist <- rep(NA, ncat)
  auxlist[params$pattern == "dot"] <- pch
  params$dot.pch <- auxlist
  rm(pch)
  
  auxlist[params$pattern == "dot"] <- rep(dot.cex, 
                                          ndots)[1:ndots]
  params$dot.cex.pch <- auxlist
  rm(dot.cex)
  
  bg <- ifelse(rep(is.null(dots$bg), ndots),
              par()$bg,
              dots$bg)
  auxlist[params$pattern == "dot"] <- bg
  params$dot.bg <- auxlist
  rm(bg, ndots)
  
  # params for Text
  ntxt <- nrow(params[params$pattern == "text", ])
  ptrn.text <- rep(ptrn.text, ntxt)[1:ntxt]
  auxlist <- rep(NA, ncat)
  auxlist[params$pattern == "text"] <- ptrn.text
  params$text.value <- auxlist
  rm(ptrn.text)
  
  text.cex <- rep(text.cex, ntxt)[1:ntxt]
  auxlist[params$pattern == "text"] <- text.cex
  params$text.cex <- auxlist
  rm(text.cex, ntxt)
  #Reversing table 
  params <- params[nrow(params):1,]
  # End params table
  
  # exit for none
  positions <- c("bottomleft","topleft",
                 "topright","bottomright",
                 "left","right","top",
                 "bottom","center","bottomleftextra"
  )
  if (length(pos) == 1) {
    if (!pos %in% positions) {
      return(invisible())
    }
  }
  
  # figdim in geo coordinates
  x1 <- par()$usr[1]
  x2 <- par()$usr[2]
  y1 <- par()$usr[3]
  y2 <- par()$usr[4]
  
  # offsets
  delta1 <- xinch(0.15) * cex
  delta2 <- delta1 / 2
  
  # variables internes
  width <- (x2 - x1) / (30 / cex)
  height <- width / 1.5
  
  # xsize
  categ <- params$categ
  
  longVal <- categ[
    strwidth(categ, cex = values.cex) == max(strwidth(categ, cex = values.cex))
    ][1]
  longVal <- max(strwidth(c(longVal), cex = values.cex))
  legend_xsize <- max(width + longVal,
                      strwidth(title.txt,
                               cex = title.cex) - delta2
  ) - delta2
  # ysize
  legend_ysize <-
    (length(categ)) * height + delta2 * (length(categ)) +
    strheight(title.txt, cex = title.cex) - delta2
  
  
  
  # Get legend position
  legcoord <- legpos(pos = pos,
                     x1 = x1,
                     x2 = x2,
                     y1 = y1,
                     y2 = y2,
                     delta1 = delta1,
                     delta2 = delta2,
                     legend_xsize = legend_xsize,
                     legend_ysize = legend_ysize
  )
  xref <- legcoord$xref
  yref <- legcoord$yref
  
  # Frame
  if (frame == TRUE) {
    rect(xref - delta1,
         yref - delta1,
         xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2,
         border = "black",
         col = "white"
    )
  }
  
  for (i in 0:(length(categ) - 1)) {
    j <- i + 1
    
    # Overlay pattern
    rect <- c(xref,
             yref + i * height + i * delta2,
             xref + width,
             yref + height + i * height + i * delta2)
    
    class(rect) <- "bbox"
    rect <- sf::st_as_sfc(rect)
    plot(
      sf::st_geometry(rect),
      col = params$legendfill[j],
      border = "black",
      lwd = 0.4,
      add = T
    )
    
    if (params$pattern[j] == "text") {
      centre <- sf::st_centroid(rect) 
      centre <- sf::st_coordinates(centre)
      text(x = centre[1],
           y = centre[2],
           labels = params$text.value[j],
           col = params$col[j],
           cex = as.double(params$text.cex[j])
      )
    } else if (params$pattern[j] == "dot") {
      fr <- sf::st_make_grid(rect, 
                            n = c(2, 2)*params$density[j], 
                            what = "centers")
      plot(sf::st_geometry(fr),
           pch = as.integer(params$dot.pch[j]),
           cex = as.double(params$dot.cex.pch[j]),
           col = params$col[j],
           bg = params$dot.bg[j],
           add = T
      )
    } else {
      patt <- hatchedLayer(rect,
                          pattern = params$pattern[j],
                          mode = "legend",
                          density = params$density[j])
      plot(sf::st_geometry(patt),
           add = T,
           col = params$col[j],
           lwd = as.double(params$line.lwd[j]),
           lty = as.integer(params$line.lty[j])
      )
      # Add border #
      plot(sf::st_geometry(rect),
           add = T,
           col = NA,
           border = "black",
           lwd = 0.4
      )
    }
    
    
    # Label Legend
    text(x = xref + width + delta2 ,
         y = yref + height / 2 + i * height + i * delta2,
         labels = params$categ[j],
         adj = c(0, 0.5),
         cex = values.cex
    )
  }
  
  
  # Affichage du titre
  text(
    x = xref,
    y = yref + length(categ) * height + length(categ) * delta2 + delta2,
    labels = title.txt,
    adj = c(0, 0),
    cex = title.cex
  )
}
