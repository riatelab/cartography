#' @title Plot credits
#' @description Plot credits (sources, author, year...).
#' @name tc_credits
#' @eval my_params(c('col'))
#' @param pos position, one of 'bottomleft', 'bottomright' or 'rightbottom'
#' @param txt text of the credits, use '\\n' to add line breaks
#' @param cex cex of the credits
#' @param font font of the credits
#' @export
#'
#' @examples
#' mtq <- tc_import_mtq()
#' tc_map(mtq)
#' tc_credits(txt = "Author\nSources - Year")
tc_credits <- function(txt, pos = "bottomleft", col, cex = .6, font = 3) {
  if (missing(col)) {
    col <- .gmapsf$args$fg
  }
  pd <- par("usr")
  pw <- pd[2] - pd[1]
  pdp <- strwidth("M", units = "user", cex = 1) / 2
  
  parapos <- switch(pos,
                    bottomleft = {
                      pd[1] <- pd[1] + pdp
                      pd[3] <- pd[3] + pdp
                      list(pd = pd, adj = c(0, 0), srt = 0)
                    },
                    bottomright = {
                      pd[1] <- pd[2] - pdp
                      pd[3] <- pd[3] + pdp
                      list(pd = pd, adj = c(1, 0), srt = 0)
                    },
                    rightbottom = {
                      pd[1] <- pd[2] - pdp
                      pd[3] <- pd[3] + pdp
                      list(pd = pd, adj = c(0, 0), srt = 90)
                    }
  )
  
  pd <- parapos$pd
  adj <- parapos$adj
  srt <- parapos$srt
  text(
    x = pd[1], y = pd[3], labels = txt,
    cex = cex, xpd = TRUE, adj = adj,
    col = col, srt = srt, font = font
  )

  
}
