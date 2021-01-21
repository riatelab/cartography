#' @title Plot proportional symbols using typology coloration
#' @description Plot proportional symbols with colors based on qualitative data.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'inches', 'val_max', 'symbol', 'col_na', 'pal', 'leg_val_rnd',
#' 'leg_pos', 'leg_title', 'leg_title_cex', 'leg_val_cex', 'val_order',
#' 'leg_no_data', 'leg_frame'))
#'
#' @importFrom methods is
#' @export
#'
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map(mtq)
#' tc_map_pt(mtq, c("POP", "STATUS"))
#'
#' mtq[6, "STATUS"] <- NA
#' tc_map(mtq)
#' tc_map_pt(
#'   x = mtq, var = c("POP", "STATUS"), inches = .35, border = "tomato4",
#'   val_max = 90000, symbol = "circle", col_na = "grey", pal = "Dynamic",
#'   lwd = 2,
#'   leg_pos = c("bottomright", "bottomleft"),
#'   leg_title = c("Population", "Municipality\nstatus"),
#'   leg_title_cex = c(0.9, 0.9),
#'   leg_val_cex = c(.7, .7),
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   leg_no_data = "No dada",
#'   leg_frame = c(TRUE, TRUE),
#'   add = TRUE
#' )
tc_map_pt <- function(x, var,
                      inches = 0.3,
                      val_max,
                      symbol = "circle",
                      pal = "Dynamic",
                      val_order,
                      border,
                      lwd = .7,
                      col_na = "white",
                      leg_pos = tc_get_leg_pos(x,2),
                      leg_title = var,
                      leg_title_cex = c(.8, .8),
                      leg_val_cex = c(.6, .6),
                      leg_val_rnd = c(0),
                      leg_no_data = "No data",
                      leg_frame = c(FALSE, FALSE),
                      add) {
  # default
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  bg <- .gmapsf$args$bg
  fg <- .gmapsf$args$fg
  if (missing(border)) border <- fg
  if (missing(add)) add <- TRUE
  
  var2 <- var[2]
  var1 <- var[1]
  # check merge and order
  dots <- create_dots(x = x, var = var1)
  
  # get modalities
  val_order <- get_modalities(
    x = dots[[var2]],
    val_order = val_order
  )
  # get color list and association
  pal <- get_the_pal(pal = pal, nbreaks = length(val_order))
  # get color vector
  mycols <- get_col_typo(
    x = dots[[var2]], pal = pal,
    val_order = val_order
  )
  
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
    dots[1, var1] <- val_max
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
    val_rnd = leg_val_rnd,
    frame = leg_frame[1], border = border, lwd = lwd, bg = bg, fg = fg
  )
  
  tc_leg_t(
    pos = leg_pos[2], val = val_order, title = leg_title[2],
    title_cex = leg_title_cex[2], val_cex = leg_val_cex[2],
    col_na = col_na, no_data = no_data, no_data_txt = leg_no_data,
    frame = leg_frame[2], pal = pal, bg = bg, fg = fg
  )
  
  return(invisible(NULL))
}
