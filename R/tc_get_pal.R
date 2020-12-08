#' @title Get color palettes
#' @description \code{tc_get_pal} builds sequential, diverging and
#' qualitative color palettes.
#' Diverging color palettes can be dissymmetric (different number of colors in
#' each of the two gradients).
#' @name tc_get_pal
#' @param n the number of colors (>= 1) to be in the palette.
#' @param palette a valid palette name (one of hcl.pals()). The name is matched to
#' the list of available palettes, ignoring upper vs. lower case, spaces, dashes,
#' etc. in the matching.
#' @param alpha an alpha-transparency level in the range [0,1] (0 means
#' transparent and 1 means opaque), see argument alpha in hsv and hcl, respectively.
#' @param rev	logical indicating whether the ordering of the colors should be reversed.
#' @param neutral a color, if two gradients are used, the 'neutral' color can be
#' added between them.
#' @details See \link{hcl.pals} to get available palette names.
#' If two gradients are used, the 'neutral' color can be added between them.
#'
#' @return \code{tc_get_pal} returns a vector of colors.
#' @importFrom grDevices hcl.colors
#' @export
#' @examples
#' cols <- tc_get_pal(n = 10, pal = "Reds 2")
#' plot(1:10, rep(1,10), bg = cols, pch = 22, cex = 4)
#' cols <- tc_get_pal(n = c(3,7), pal = c("Reds 2", "Greens"))
#' plot(1:10, rep(1,10), bg = cols, pch = 22, cex = 4)
#' cols <- tc_get_pal(n = c(5,5), pal = c("Reds 2", "Greens"))
#' plot(1:10, rep(1,10), bg = cols, pch = 22, cex = 4)
#' cols <- tc_get_pal(n = c(7,3), pal = c("Reds 2", "Greens"))
#' plot(1:10, rep(1,10), bg = cols, pch = 22, cex = 4)
#' cols <- tc_get_pal(n = c(5,5), pal = c("Reds 2", "Greens"), neutral = "grey")
#' plot(1:11, rep(1,11), bg = cols, pch = 22, cex = 4)
#' opar <- par(bg = "black")
#' cols <- tc_get_pal(n = c(7,3), pal = c("Reds 2", "Greens"), alpha = c(.3,.7))
#' plot(1:10, rep(1,10), bg = cols, pch = 22, cex = 4)
#' par(opar)
#' cols <- tc_get_pal(n = c(5,5), pal = c("Reds 2", "Greens"), rev = c(TRUE, TRUE))
#' plot(1:10, rep(1,10), bg = cols, pch = 22, cex = 4)
tc_get_pal <- function(n, palette, alpha = NULL, rev = c(FALSE,FALSE), neutral) {
  if (length(n) == 1) {
    pal <- hcl.colors(n = n, palette = palette,
                      alpha = alpha, rev = rev[1])
  }
  
  if (length(n) == 2) {
    nmax <- max(n)
    
    pal1 <- hcl.colors(n = nmax, palette = palette[1], alpha = alpha[1], rev = rev[1])
    pal2 <- hcl.colors(n = nmax, palette = palette[2], alpha = alpha[2], rev = rev[2])
    
    if(n[1] < nmax){
      pal1 <- pal1[(nmax - n[1] + 1):nmax]
    }else{
      pal1 <- pal1[1:nmax]
    }
    pal2 <- rev(pal2)
    pal2 <- pal2[1:n[2]]
    pal <- c(pal1, pal2)
    if (!missing(neutral)) {
      pal <- c(pal1, neutral, pal2)
    }
  }
  return(pal)
}
