#' @title Plot a typology map
#' @description Plot a typology map.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'col_na', 'pal',
#' 'leg_pos', 'leg_title', 'leg_title_cex', 'leg_val_cex', 'val_order',
#' 'leg_no_data', 'leg_frame'))
#' @param cex cex cex of the symbols if x is a POINT layer
#' @param pch pch type of pch if x is a POINT layer
#' @param pch_na pch for NA values if x is a POINT layer
#' @param cex_na cex for NA values if x is a POINT layer
#' @importFrom methods is
#' @export
#'
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map_t(mtq, "STATUS")
#' mtq[6, "STATUS"] <- NA
#' tc_map_t(
#'   x = mtq, var = "STATUS", pal = c("red", "blue", "yellow"), lwd = 1.1,
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   col_na = "green", border = "brown",
#'   leg_pos = "bottomleft",
#'   leg_title = "Status", leg_title_cex = 1.1,
#'   leg_val_cex = 1, leg_no_data = "No data",
#'   leg_frame = TRUE, add = FALSE
#' )
tc_map_t <- function(x,
                     var,
                     pal = "Dynamic",
                     val_order,
                     border,
                     pch = 21,
                     cex = 1,
                     lwd = .7,
                     cex_na = 1,
                     pch_na = 4,
                     col_na = "white",
                     leg_pos = "topright",
                     leg_title = var,
                     leg_title_cex = .8,
                     leg_val_cex = .6,
                     leg_no_data = "No data",
                     leg_frame = FALSE,
                     add) {
  # default
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  bg <- .gmapsf$args$bg
  fg <- .gmapsf$args$fg
  if (missing(border)) border <- fg
  if (missing(add)) add <- FALSE
  
  # get modalities
  val_order <- get_modalities(
    x = x[[var]],
    val_order = val_order
  )
  
  # get color list and association
  pal <- get_the_pal(pal = pal, nbreaks = length(val_order))
  # get color vector
  mycols <- get_col_typo(
    x = x[[var]], pal = pal,
    val_order = val_order
  )
  
  no_data <- FALSE
  if (max(is.na(mycols)) == 1) {
    no_data <- TRUE
  }
  mycols[is.na(mycols)] <- col_na
  
  if (is(st_geometry(x), c("sfc_LINESTRING", "sfc_MULTILINESTRING"))) {
    plot(st_geometry(x), col = mycols, lwd = lwd, bg = bg, add = add)
    tc_leg_t(
      pos = leg_pos, val = val_order, title = leg_title,
      title_cex = leg_title_cex, val_cex = leg_val_cex,
      col_na = col_na, no_data = no_data, no_data_txt = leg_no_data,
      frame = leg_frame, pal = pal, bg = bg, fg = fg
    )
  }
  if (is(st_geometry(x), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
    plot(st_geometry(x),
         col = mycols, border = border,
         lwd = lwd, bg = bg, add = add
    )
    tc_leg_t(
      pos = leg_pos, val = val_order, title = leg_title,
      title_cex = leg_title_cex, val_cex = leg_val_cex,
      col_na = col_na, no_data = no_data, no_data_txt = leg_no_data,
      frame = leg_frame, pal = pal, bg = bg, fg = fg
    )
  }
  if (is(st_geometry(x), c("sfc_POINT", "sfc_MULTIPOINT"))) {
    if (pch %in% 21:25){
      mycolspt <- border
    } else {
      mycolspt <- mycols
    }
    mycolsptbg <- mycols
    plot(st_geometry(x),
         col = mycolspt, bg = mycolsptbg, cex = cex, pch = pch,
         lwd = lwd, add = add
    )
    n <- length(val_order)
    tc_leg_s(
      pos = leg_pos, val = val_order, title = leg_title,title_cex = leg_title_cex,
      val_cex = leg_val_cex, col_na = col_na, no_data = no_data, no_data_txt = leg_no_data,
      frame = leg_frame, border = border, pal = pal, lwd = lwd,
      pt_cex = rep(cex, n), pt_pch = rep(pch, n), pt_cex_na = cex_na,
      pt_pch_na = pch_na, bg = bg, fg = fg
    )
  }
  
  ################# FIX NA PB ################################"
  
  # box(col = bg)
  
  
  
  return(invisible(NULL))
}