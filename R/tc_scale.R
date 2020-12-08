#' @title Plot a scale bar
#' @description Plot a scale bar.
#' @name tc_scale
#' @eval my_params(c("col"))
#' @param size size of the scale bar in units (default to km). If size is not
#' set, an automatic size is used (1/10 of the map width)
#' @param lwd width of the scale bar
#' @param cex cex of the text
#' @param pos position of the legend, default to "bottomright".
#' "bottomright" or a vector of two coordinates (c(x, y)) are possible.
#' @param unit units used for the scale bar. Can be "mi" for miles,
#' "m" for meters, or "km" for kilometers (default)
#' @note This scale bar is not accurate on unprojected (long/lat) maps.
#' @export
#' @examples
#' mtq <- tc_import_mtq()
# tc_map(mtq)
# tc_scale()
tc_scale <- function(size, pos = "bottomright",
                     lwd = 1.5, cex = 0.6, col, unit = "km") {
  # default color
  if (missing(col)) {
    col <- .gmapsf$args$fg
  }

  # get the current plot dimensions
  pu <- par("usr")
  inset <- strwidth("M", units = "user", cex = 1) / 2


  # default scale
  if (missing(size)) {
    size <- diff(pu[1:2]) / 10
    size <- unit_conversion(size = size, unit_in = "m", unit_out = unit)
    size_text <- signif(size, digits = 0)
    size <- unit_conversion(size = size_text, unit_in = unit, unit_out = "m")
  } else {
    # convert distance into meters based on dist_unit
    size_text <- as.character(size)
    size <- unit_conversion(size, unit_in = unit, unit_out = "m")
  }

  # label
  labelscale <- paste0(size_text, " ", unit)

  # xy pos
  xscale <- pu[2] - inset - size
  yscale <- pu[3] + inset

  if (!missing(pos)) {
    if (is.numeric(pos) & length(pos) == 2) {
      xscale <- pos[1]
      yscale <- pos[2]
    } else {
      if (pos == "bottomleft") {
        xscale <- pu[1] + inset
        yscale <- pu[3] + inset
      }
    }
  }

  # plot the scale bar
  segments(
    x0 = xscale,
    y0 = yscale,
    x1 = xscale + size,
    y1 = yscale,
    lwd = lwd,
    col = col, xpd = T
  )
  # plot the scale bar label
  text(xscale + (size / 2), yscale,
    adj = c(0.5, -.5),
    labels = labelscale,
    cex = cex, col = col,
    xpd = TRUE
  )



}


#' Convert units
#' @param size a size
#' @param unit_in input unit
#' @param unit_out output unit
#' @noRd
unit_conversion <- function(size, unit_in, unit_out) {
  # uncomment comments if the function is eventually exported

  # if(!unit_in %in% c('km','m','mi')) stop("unit must be 'km', 'm', or 'mi'")
  if (!unit_out %in% c("km", "m", "mi")) stop("unit must be 'km', 'm', or 'mi'")

  if (unit_out == "m") {
    if (unit_in == "km") size <- size * 1000
    if (unit_in == "mi") size <- size * 1609.344
  }
  if (unit_out == "km") {
    if (unit_in == "m") size <- size / 1000
    # if(unit_in == "mi") size <- size * 1.609344
  }
  if (unit_out == "mi") {
    if (unit_in == "m") size <- size / 1609.344
    # if(unit_in == "km") size <- size / 1.609344
  }
  return(size)
}
