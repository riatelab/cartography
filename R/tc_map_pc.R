#' @title Plot proportional symbols using choropleth coloration
#' @description Plot proportional symbols with colors based on a quantitative
#' data classification.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'inches', 'val_max', 'symbol', 'col_na', 'pal', 'breaks', 'nbreaks',
#' 'leg_pos', 'leg_title', 'leg_title_cex', 'leg_val_cex', 'leg_val_rnd',
#' 'leg_no_data', 'leg_frame'))
#'
#' @importFrom methods is
#' @export
#'
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map(mtq) 
#' tc_map_pc(mtq, c("POP", "MED"))
#'
#' tc_map(mtq)
#' mtq[6, "MED"] <- NA
#' tc_map_pc(
#'   x = mtq, var = c("POP", "MED"), inches = .35, border = "tomato4",
#'   val_max = 90000, symbol = "circle", col_na = "grey", pal = "Cividis",
#'   breaks = "equal", nbreaks = 4, lwd = 4,
#'   leg_pos = c("bottomright", "bottomleft"),
#'   leg_title = c("Population", "Median Income"),
#'   leg_title_cex = c(0.8, 1),
#'   leg_val_cex = c(.7, .9),
#'   leg_val_rnd = c(0, 0),
#'   leg_no_data = "No data",
#'   leg_frame = c(TRUE, TRUE),
#'   add = TRUE
#' )
tc_map_pc <- function(x,
                      var,
                      inches = 0.3,
                      val_max,
                      symbol = "circle",
                      pal = "Mint",
                      breaks = "quantile",
                      nbreaks,
                      border,
                      lwd = .7,
                      col_na = "white",
                      leg_pos = tc_get_leg_pos(x,2),
                      leg_title = var,
                      leg_title_cex = c(.8, .8),
                      leg_val_cex = c(.6, .6),
                      leg_val_rnd = c(0, 2),
                      leg_no_data = "No data",
                      leg_frame = c(FALSE, FALSE),
                      add) {
  # default
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  bg <- .gmapsf$args$bg
  fg <- .gmapsf$args$fg
  if (missing(add)) add <- TRUE
  if (missing(border)) border <- fg
  
  var2 <- var[2]
  var1 <- var[1]
  # check merge and order
  dots <- create_dots(x = x, var = var1)
  
  # get the breaks
  breaks <- tc_get_breaks(
    x = dots[[var2]], nbreaks = nbreaks,
    breaks = breaks
  )
  nbreaks <- length(breaks) - 1
  # get the cols
  pal <- get_the_pal(pal = pal, nbreaks = nbreaks)
  # get the color vector
  mycols <- get_col_vec(x = dots[[var2]], breaks = breaks, pal = pal)
  
  no_data <- FALSE
  if (max(is.na(mycols)) == 1) {
    no_data <- TRUE
  }
  mycols[is.na(mycols)] <- col_na
  
  # Default max value
  if (missing(val_max)) {
    val_max <- max(dots[[var1]])
  }
  
  # get sizes
  sizes <- get_size(
    var = dots[[var1]], inches = inches,
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
  
  # symbols size
  val <- seq(sqrt(min(dots[[var1]])), sqrt(max(dots[[var1]])), length.out = 4)
  val <- val * val
  tc_leg_p(
    pos = leg_pos[1], val = val, title = leg_title[1],
    symbol = symbol, inches = inches, col = "grey80",
    title_cex = leg_title_cex[1], val_cex = leg_val_cex[1],
    val_rnd = leg_val_rnd[1],
    frame = leg_frame[1], border = border, lwd = lwd,
    bg = bg, fg = fg
  )
  tc_leg_c(
    pos = leg_pos[2], val = breaks, title = leg_title[2],
    title_cex = leg_title_cex[2], val_cex = leg_val_cex[2],
    val_rnd = leg_val_rnd[2],
    col_na = col_na, no_data = no_data, no_data_txt = leg_no_data,
    frame = leg_frame[2], pal = pal, bg = bg, fg = fg
  )
  
  return(invisible(NULL))
}
