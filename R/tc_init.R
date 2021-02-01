#' @title Initialize or export a map
#' @name tc_init
#' @description Plot an invisible layer with the extent of a spatial object.
#' Export a map in png or svg format. It uses a reference
#' geographic layer to find a correct ratio for the export.
#' @eval my_params(c("xfull", "bg"))
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param shadow add the shadow of x
#' @param shadow_col color of the shadow
#' @param shadow_cex shadow extent (1 means 1 percent of the shortest side of
#' the bounding box)
#' @param theme apply a theme from \code{tc_theme}
#' @param filename path to the exported file
#' @param export if set to "png" or "svg" a png or svg plot device is opened
#' @param width width of the figure (pixels for png, inches for svg)
#' @param height height of the figure (pixels for png, inches for svg)
#' @param res resolution (for png)
#' @export
#' @importFrom grDevices png svg
#' @importFrom sf st_bbox st_as_sfc st_geometry
#' @examples
#' mtq <- tc_import_mtq()
#' target <- mtq[30, ]
#' tc_init(target, bg = "lightblue")
#' tc_map(mtq, add = TRUE)
tc_init <- function(x,
                    expandBB = rep(0, 4),
                    bg,
                    shadow = FALSE,
                    shadow_col = "grey50",
                    shadow_cex = 1,
                    theme,
                    export,
                    filename,
                    width,
                    height,
                    res = 72) {
  if (!missing(theme)) {
    tc_theme(theme)
    mar <- .gmapsf$args$mar
  } else {
    mar <- par("mar")
  }
  
  if (missing(bg)) bg <- .gmapsf$args$bg
  
  # transform to bbox
  bb <- st_bbox(x)
  y <- st_as_sfc(bb)
  
  if (par("xaxs") == "r") {
    expandBB <- expandBB / (1 + 0.08)
  }
  # expandBB mgmt
  extra <- expandBB[c(2, 1, 4, 3)]
  w <- bb[3] - bb[1]
  h <- bb[4] - bb[2]
  bb <- bb + (extra * c(-w, -h, w, h))
  
  if (!missing(export)) {
    if (export == "png") {
      if (missing(width) & missing(height)) {
        width <- 600
      }
      fd <- get_ratio(
        x = bb, width = width, height = height,
        mar = mar, res = res, format = "png"
      )
      png(filename, width = fd[1], height = fd[2], res = res)
    }
    if (export == "svg") {
      if (missing(height) & missing(width)) {
        width <- 7
      }
      if (!missing(width) && width > 50) {
        message(paste0(
          "It is unlikely that you really want to produce a figure",
          " with more than 50 inches of width.", " The width has been",
          " set to 7 inches."
        ))
        width <- 7
      }
      fd <- get_ratio(
        x = bb,
        width =  width,
        height = height,
        mar = mar, res = res, format = "svg"
      ) / 96
      svg(filename = filename, width = fd[1], height = fd[2])
    }
    if (!missing(theme)) {
      tc_theme(theme)
    }
  }
  
  # margins mgmt
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  
  # plot with bg and margins
  plot(y, col = NA, bg = bg, border = NA, lwd = 1, expandBB = expandBB, asp = 1)
  
  # add a shadow
  if (shadow) {
    d <- shadow_cex * strwidth("M", units = "user", cex = 1) / 3
    z <- st_geometry(x) + c(d, -d)
    plot(z, col = shadow_col, border = shadow_col, lwd = .5, add = TRUE)
  }
  
}







get_ratio <- function(x, width, height, mar, res, format) {
  iw <- x[3] - x[1]
  ih <- x[4] - x[2]
  if (missing(height)) {
    if (format == "svg"){
      width <- width * 96
    }
    wh <- iw / ih
    widthmar <- width - (0.2 * (mar[2] + mar[4]) * res)
    height <- (widthmar / wh) + (0.2 * (mar[1] + mar[3]) * res)
  } else {
    if (format == "svg"){
      height <- height * 96
    }
    hw <- ih / iw
    heightmar <- height - (0.2 * (mar[1] + mar[3]) * res)
    width <- (heightmar / hw) + (0.2 * (mar[2] + mar[4]) * res)
  }
  return(unname(floor(c(width, height))))
}
