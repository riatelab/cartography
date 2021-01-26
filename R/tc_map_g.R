#' @title Plot graduated symbols
#' @description Plot graduated symbols based on quantitative data.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'col',
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_frame', 
#' 'breaks',
#' 'nbreaks'))
#' @param cex cex for symbols
#' @param pch pch for symbols
#' @importFrom methods is
#' @importFrom graphics box
#' @export
#'
#' @examples
#' mtq <- tc_import_mtq()
tc_map_g <- function(x, 
                     var,
                     breaks = "quantile",
                     nbreaks = 3,
                     col = "tomato4",
                     border,
                     pch = 21,
                     cex,
                     lwd,
                     leg_pos = tc_get_leg_pos(x),
                     leg_title = var,
                     leg_title_cex = .8,
                     leg_val_cex = .6,
                     leg_val_rnd = 2,
                     leg_frame = FALSE,
                     add) {
  # default
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  bg <- .gmapsf$args$bg
  fg <- .gmapsf$args$fg
  if (missing(border)) border <- fg
  if (missing(add)) add <- TRUE
  
  # data prep
  x <- x[!is.na(x = x[[var]]), ]
  x <- x[order(x[[var]], decreasing = TRUE), ]
  breaks <- tc_get_breaks(x = x[[var]], nbreaks = nbreaks, breaks = breaks)
  nbreaks <- length(breaks) - 1
  
  if (is(st_geometry(x), c("sfc_LINESTRING", "sfc_MULTILINESTRING"))) {
    # lwd mgmt
    if(missing(lwd)){
      lwd <- seq(1, 4, length.out = nbreaks)
    }
    if(length(lwd) != nbreaks){
      stop(paste0("the length of lwd does not match the number of ",
                  "breaks."), call. = FALSE)
    }
    mylwd <- get_col_vec(
      x = x[[var]], breaks = breaks, pal = lwd
    )
    # map
    plot(sf::st_geometry(x), col = col, lwd = mylwd, add = add)
    # legend
    tc_leg_gl(pos = leg_pos, val = breaks, title = leg_title,
              title_cex = leg_title_cex ,val_cex = leg_val_cex,
              val_rnd =  leg_val_rnd, lwd = lwd, col = col, 
              bg = bg, fg =fg, frame = leg_frame)
    return(invisible(NULL))
  }
  
  if(missing(lwd)) lwd <- .7
  
  # Transform to point
  st_geometry(x) <- st_centroid(st_geometry(x), of_largest_polygon = TRUE)
  # cex mgmt 
  if(missing(cex)){
    cex <- seq(1, 4, length.out = nbreaks)
  }
  if(length(cex) != nbreaks){
    stop(paste0("the length of cex does not match the number of ",
                "breaks."), call. = FALSE)
  }
  mycex <- get_col_vec(
    x = x[[var]], breaks = breaks, pal = cex
  )
  # color mgmt
  pch <- pch[1]
  mycolspt <- col[1]
  if (pch %in% 21:25) mycolspt <- border
  mycolsptbg <- col[1]
  
  # display
  plot(st_geometry(x),
       col = mycolspt, bg = mycolsptbg, cex = mycex, pch = pch,
       lwd = lwd, add = add
  )
  # legend
  lb <- length(breaks)
  lab <- paste0(breaks[1:(lb-1)], rep(" - ", lb-1) , breaks[2:lb])
  tc_leg_s(
    pos = leg_pos, val = rev(lab), title = leg_title,title_cex = leg_title_cex,
    val_cex = leg_val_cex, 
    frame = leg_frame, border = border, pal = col, lwd = lwd,
    pt_cex = rev(cex), pt_pch = pch, bg = bg, fg = fg
  )
  return(invisible(NULL))
}
