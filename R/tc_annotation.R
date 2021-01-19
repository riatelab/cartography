#' Plot an annotation on a map
#'
#' @param x an sf object with 1 row, a couple of coordinates (c(x, y)). 
#' @param txt the text to display
#' @param pos position of the text, one of "topleft", "topright", "bottomright", 
#' "bottomleft"
#' @param cex size of the text
#' @param col_arrow arrow color
#' @param col_txt text colot
#' @param halo add a halo around the text
#' @param bg halo color
#' @param s arrow size (min=1)
#' @param ... further \link{text} arguments.
#' @export
#'
#' @examples
#' mtq <- tc_import_mtq()
#' tc_theme("nevermind")
#' tc_map(mtq)
#' tc_annotation(x = c(711167.8,1614764 ), 
#'               txt = "Look!\nImportant feature\nhere!", 
#'               pos = "bottomleft", cex = 1.2, font = 2,
#'               halo = TRUE, s = 1.5 )
#' 
#' tc_annotation(x = mtq[1, ], 
#'               txt = "This is less\nimportant", 
#'               cex = .7, font = 3, s = 1.3 )
tc_annotation <- function(x, txt, pos = "topright", 
                          cex = 0.8, col_arrow, 
                          col_txt, halo = FALSE, bg, s = 1,... ){
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  
  if(missing(col_arrow)){
    col_arrow <- "black"
  }
  if(missing(col_txt)){
    col_txt <- .gmapsf$args$fg
  }
  if(missing(bg)){
    bg <- .gmapsf$args$bg
  }
  
  
  
  if(inherits(x, c('sf', 'sfc'))){
    xy <- sf::st_coordinates(
      sf::st_centroid(
        sf::st_geometry(x[1,]), 
        of_largest_polygon = TRUE
      )
    )
  }
  if(inherits(x, "numeric") && length(x) == 2){
    xy <- x
  }
  
  if (s < 1) s <- 1
  
  inset <- strwidth("M", units = "user", cex = 1) / 2
  radius = 5 * s * inset
  
  switch(pos, 
         topright = {
           drawarc(x = xy[1] + radius, 
                   y = xy[2] + inset, 
                   radius = radius, 
                   deg1 = 180, 
                   deg2 = 90, 
                   col = col_arrow)
           polygon(x = c(xy[1] - inset / 3, 
                         xy[1], 
                         xy[1] + inset / 3, 
                         xy[1] - inset / 3), 
                   y = c(xy[2] + 2 * inset, 
                         xy[2] + 5 * inset / 6, 
                         xy[2] + 2 * inset, 
                         xy[2] + 2 * inset),
                   col = col_arrow, border = col_arrow, lwd = 1.2)
           if(halo){
             shadowtext(x = xy[1] + radius + inset / 2, 
                        y = xy[2]+ radius + inset, 
                        labels = txt, col = col_txt, bg = bg, 
                        cex = cex,adj = c(0,.5), ...)
           }else{
             text(x = xy[1] + radius + inset / 2, 
                  y = xy[2]+ radius + inset, cex = cex, 
                  labels = txt, col = col_txt,
                  adj = c(0,.5), ...)
           }
         },
         topleft = {
           drawarc(x = xy[1] - radius, 
                   y = xy[2] + inset, 
                   radius = radius, 
                   deg1 = 0, 
                   deg2 = 90, 
                   col = col_arrow)
           polygon(x = c(xy[1] - inset / 3, 
                         xy[1], 
                         xy[1] + inset / 3, 
                         xy[1] - inset / 3), 
                   y = c(xy[2] + 2 * inset, 
                         xy[2] + 5 * inset / 6, 
                         xy[2] + 2 * inset, 
                         xy[2] + 2 * inset),
                   col = col_arrow, border = col_arrow, lwd = 1.2)
           if(halo){
             shadowtext(x = xy[1] - radius - inset / 2, 
                        y = xy[2] + radius + inset,  
                        labels = txt, col = col_txt, bg = bg, 
                        cex = cex,adj = c(1,.5), ...)
           }else{
             text(x = xy[1] - radius - inset / 2, 
                  y = xy[2] + radius + inset, cex = cex, 
                  labels = txt, col = col_txt,
                  adj = c(1,.5), ...)
           }
         }, 
         bottomleft = {
           drawarc(x = xy[1] - radius, 
                   y = xy[2] - inset, 
                   radius = radius, 
                   deg1 = 360, 
                   deg2 = 270, 
                   col = col_arrow)
           polygon(x = c(xy[1] - inset / 3, 
                         xy[1], 
                         xy[1] + inset / 3, 
                         xy[1] - inset / 3), 
                   y = c(xy[2] - 2 * inset, 
                         xy[2] - 5 * inset / 6, 
                         xy[2] - 2 * inset, 
                         xy[2] - 2 * inset),
                   col = col_arrow, border = col_arrow, lwd = 1.2)
           if(halo){
             shadowtext(x = xy[1] - radius - inset / 2, 
                        y = xy[2] - radius - inset,  
                        labels = txt, col = col_txt, bg = bg, 
                        cex = cex,adj = c(1,.5), ...)
           }else{
             text(x = xy[1] - radius - inset / 2, 
                  y = xy[2] - radius - inset, cex = cex, 
                  labels = txt, col = col_txt,
                  adj = c(1,.5), ...)
           }
         }, 
         bottomright = {
           drawarc(x = xy[1] + radius, 
                   y = xy[2] - inset, 
                   radius = radius, 
                   deg1 = 270, 
                   deg2 = 180, 
                   col = col_arrow)
           polygon(x = c(xy[1] - inset / 3, 
                         xy[1], 
                         xy[1] + inset / 3, 
                         xy[1] - inset / 3), 
                   y = c(xy[2] - 2 * inset, 
                         xy[2] - 5 * inset / 6, 
                         xy[2] - 2 * inset, 
                         xy[2] - 2 * inset),
                   col = col_arrow, border = col_arrow, lwd = 1.2)
           if(halo){
             shadowtext(x = xy[1] + radius + inset / 2, 
                        y = xy[2] - radius - inset, 
                        labels = txt, col = col_txt, bg = bg, 
                        cex = cex,adj = c(0,.5), ...)
           }else{
             text(x = xy[1] + radius + inset / 2, 
                  y = xy[2] - radius - inset, cex = cex, 
                  labels = txt, col = col_txt,
                  adj = c(0,.5), ...)
           }
         }
  )
}



drawarc <- function (x = 1, y = NULL, radius = 1, deg1 = 0, deg2 = 45, col) {
  n = 0.05
  angle1 = deg1 * pi/180
  angle2 = deg2 * pi/180
  lwd = 1.2
  xylim <- par("usr")
  ymult <- 1
  devunits <- grDevices::dev.size("px")
  draw.arc.0 <- function(x, y, radius, angle1, angle2, n, col, 
                         lwd, ...) {
    delta.angle <- (angle2 - angle1)
    if (n != as.integer(n)) 
      n <- as.integer(1 + delta.angle/n)
    delta.angle <- delta.angle/n
    angleS <- angle1 + seq(0, length = n) * delta.angle
    angleE <- c(angleS[-1], angle2)
    if (n > 1) {
      half.lwd.user <- (lwd/2) * (xylim[2] - xylim[1])/devunits[1]
      adj.angle = delta.angle * half.lwd.user/(2 * (radius + 
                                                      half.lwd.user))
      angleS[2:n] = angleS[2:n] - adj.angle
      angleE[1:(n - 1)] = angleE[1:(n - 1)] + adj.angle
    }
    p1x <- x + radius * cos(angleS)
    p1y <- y + radius * sin(angleS) * ymult
    p2x <- x + radius * cos(angleE)
    p2y <- y + radius * sin(angleE) * ymult
    segments(p1x, p1y, p2x, p2y, col = col, lwd = lwd, lend = 3)
  }
  xy <- grDevices::xy.coords(x, y)
  x <- xy$x
  y <- xy$y
  a1 <- pmin(angle1, angle2)
  a2 <- pmax(angle1, angle2)
  angle1 <- a1
  angle2 <- a2
  args <- data.frame(x, y, radius, angle1, angle2, n, col, 
                     lwd, stringsAsFactors = FALSE)
  for (i in 1:nrow(args)) do.call("draw.arc.0", c(args[i, ]))
}
