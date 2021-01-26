#' @title Plot proportional symbols
#' @description Plot proportional symbols.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'col',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'inches',
#' 'val_max',
#' 'symbol',
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_frame'))
#' @param lwd_max lwd_max
#' @importFrom graphics box
#' @export
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map(mtq) 
#' tc_map_p(mtq, "POP")
#'
#' tc_map(mtq)
#' tc_map_p(
#'   x = mtq, var = "POP", inches = .4, symbol = "circle", val_max = 90000,
#'   col = "tomato1", border = "blue", lwd = 1,
#'   leg_pos = "right", leg_title = "Population",
#'   leg_title_cex = 1, leg_val_cex = .8, leg_val_rnd = 0,
#'   leg_frame = TRUE, add = TRUE
#' )
tc_map_p <- function(x,
                     var,
                     inches = 0.3,
                     val_max,
                     symbol = "circle",
                     col = "tomato4",
                     border,
                     lwd = .7,
                     lwd_max = 20,
                     leg_pos = tc_get_leg_pos(x),
                     leg_title = var,
                     leg_title_cex = .8,
                     leg_val_cex = .6,
                     leg_val_rnd = 0,
                     leg_frame = FALSE,
                     add) {
  # default
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  lend <- par("lend")
  on.exit(par(op))
  bg <- .gmapsf$args$bg
  fg <- .gmapsf$args$fg
  if (missing(add)) add <- TRUE
  if (missing(border)) border <- fg

  
  
  
  if (is(st_geometry(x), c("sfc_LINESTRING", "sfc_MULTILINESTRING"))) {
    x <- x[!is.na(x[[var]]), ]
    maxval <- max(x[[var]])
    x$lwd <- x[[var]] * lwd_max / maxval
    if (add == FALSE) {
      tc_init(x, bg = bg)
    }
    
    par(lend = 1)
    tc_map(x, lwd = x$lwd, add = T, col = col)
    legendPropLines(pos = leg_pos, title.txt = leg_title, 
                    title.cex = leg_title_cex,
                    values.cex = leg_val_cex, 
                    var = c(min(x[[var]]), maxval), 
                    lwd = lwd_max, col = col, frame = leg_frame, 
                    values.rnd = leg_val_rnd)
    par(lend = lend)
    
    return(invisible(NULL))
  }
  
  
  # check merge and order
  dots <- create_dots(x = x, var = var)
  # default col
  mycols <- rep(col, nrow(dots))
  
  # Default max value
  if (missing(val_max)) {
    val_max <- max(dots[[var]])
  }
  
  # get sizes
  sizes <- get_size(
    var = dots[[var]], inches = inches,
    val_max = val_max, symbol = symbol
  )
  
  # size and values for legend, hollow circle (fixmax case)
  sizeMax <- max(sizes)
  if (inches <= sizeMax) {
    inches <- sizeMax
    borders <- border
  } else {
    mycols <- c(NA, mycols)
    borders <- c(NA, rep(border, nrow(dots)))
    dots <- rbind(dots[1, ], dots)
    dots[1, var] <- val_max
    sizes <- c(inches, sizes)
  }
  
  # empty plot if needed
  if (add == FALSE) {
    tc_init(x, bg = bg)
  }
  
  # Plot the symbols
  plot_symbols(
    symbol = symbol, dots = dots, sizes = sizes,
    mycols = mycols, border = borders, lwd = lwd,
    inches = inches
  )
  
  # if (add == FALSE) {
  #   box(bg = bg)
  # }
  
  # symbols size
  val <- seq(sqrt(min(dots[[var]])), sqrt(max(dots[[var]])), length.out = 4)
  val <- val * val
  tc_leg_p(
    pos = leg_pos, val = val, title = leg_title,
    symbol = symbol, inches = inches, col = col,
    title_cex = leg_title_cex, val_cex = leg_val_cex,
    val_rnd = leg_val_rnd,
    frame = leg_frame, border = border, lwd = lwd,
    bg = bg, fg = fg
  )
  
  return(invisible(NULL))
}