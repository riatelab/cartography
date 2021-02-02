#' @title Plot an sf object
#' @description Plot an sf object. This is mostly a wrapper around
#' \code{plot(st_geometry(x), ...)}.
#' @eval my_params(c(
#' 'xfull',
#' 'col',
#' 'border',
#' 'lwd',
#' 'bg',
#' 'add' ))
#' @param ... further parameters from \link{plot} for sfc objects
#'
#' @importFrom methods is
#' @importFrom sf st_geometry
#' @export
#'
#' @examples
#' library(sf)
#' mtq <- tc_import_mtq()
#' tc_map(mtq)
#' tc_map(mtq, bg = "green", col = "blue")
tc_map <- function(x,
                   col = "grey80",
                   border = "grey20",
                   lwd = .7,
                   bg,
                   add = FALSE,
                   ...) {
  # margins mgmt
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))

  if (missing(bg)) {
    bg <- .gmapsf$args$bg
  }

  plot(st_geometry(x),
       col = col, border = border,
       lwd = lwd, add = add, bg = bg,
       asp = 1,  ...
  )
  pu <- par("usr")
  rect(pu[1], pu[3], pu[2], pu[4], border = bg, col =  NA)

    return(invisible(NULL))
  
}
