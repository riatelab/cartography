#' @title Set a theme
#' @description This function set a color theme.
#' @param x name of a map theme. One of "default", "brutal", "ink",
#' "dark", "agolalight", "candy", "darkula", "iceberg", "green", "nevermind",
#' "jsk".
#' It is also possible to set a custom theme (see Examples).
#' The parameters set by this function are the figure margins, background and
#' foreground colors and some \link{tc_title} options.
#' Use \code{tc_theme('default')} to reset theme settings.
#' @return The (invisible) list of parameters of the current theme.
#' @export
#' @examples
#' mtq <- tc_import_mtq()
#' tc_theme("green")
#' tc_map(mtq)
#' tc_title()
#'
#' custom <- list(
#'   name = "custom",
#'   bg = "green",
#'   fg = "red",
#'   mar = c(2, 2, 2, 2),
#'   tab = TRUE,
#'   pos = "center",
#'   inner = TRUE,
#'   line = 2,
#'   cex = 1.5,
#'   font = 3
#' )
#' tc_theme(custom)
#' tc_map(mtq)
#' tc_title()
tc_theme <- function(x) {
  if (missing(x)) {
    return(.gmapsf$args)
  }
  
  themes <- list(
    default = list(
      name = "default",
      bg = "white",
      fg = "black",
      mar = c(5.1, 4.1, 4.1, 2.1),
      tab = TRUE,
      pos = "left",
      inner = FALSE,
      line = 1.2,
      cex = 1,
      font = 2
    ),
    brutal = list(
      name = "brutal",
      bg = "#FFFFFF",
      fg = "#3b4252",
      mar = c(5.1, 4.1, 4.1, 2.1),
      tab = TRUE,
      pos = "left",
      inner = FALSE,
      line = 2,
      cex = 1.5,
      font = 2
    ),
    ink = list(
      name = "ink",
      bg = "#FFDEAD",
      fg = "#0000FF",
      mar = c(0, 0, 1.2, 0),
      tab = FALSE,
      pos = "left",
      inner = FALSE,
      line = 1.2,
      cex = .9,
      font = 2
    ),
    dark = list(
      name = "dark",
      bg = "#2E3947",
      fg = "#7E848C",
      mar = c(0.5, 0.5, 2, 0.5),
      tab = FALSE,
      pos = "left",
      inner = FALSE,
      line = 1.5,
      cex = 1,
      font = 1
    ),
    agolalight = list(
      name = "agolalight",
      bg = "#EDF4F5",
      fg = "#82888A",
      mar = c(0, 0, 2, 0),
      tab = FALSE,
      pos = "left",
      inner = FALSE,
      line = 2,
      cex = 1.5,
      font = 3
    ),
    candy = list(
      name = "candy",
      bg = "#FDFCFE",
      fg = "#6B1767",
      mar = c(0, 0, 2, 0),
      tab = FALSE,
      pos = "center",
      inner = FALSE,
      line = 2,
      cex = 1.5,
      font = 2
    ),
    darkula = list(
      name = "darkula",
      bg = "#232525",
      fg = "#A9B7C6",
      mar = c(0.5, 0.5, 0.5, 0.5),
      tab = TRUE,
      pos = "right",
      inner = TRUE,
      line = 1.5,
      cex = 1,
      font = 4
    ),
    iceberg = list(
      name = "iceberg",
      bg = "#0B0E0E",
      fg = "#BDD6DB",
      mar = c(0.5, 0.5, 0.5, 0.5),
      tab = TRUE,
      pos = "right",
      inner = TRUE,
      line = 1.5,
      cex = 1,
      font = 4
    ),
    green = list(
      name = "green",
      bg = "#1B1D16",
      fg = "#D7FF68",
      mar = c(0.5, 0.5, 2, 0.5),
      tab = FALSE,
      pos = "center",
      inner = FALSE,
      line = 1.5,
      cex = 1,
      font = 2
    ),
    nevermind = list(
      name = "nevermind",
      bg = "#4DB8DA",
      fg = "#121725",
      mar = c(2, 2, 3.5, 2),
      tab = FALSE,
      pos = "center",
      inner = FALSE,
      line = 1.5,
      cex = 1.4,
      font = 1
    ),
    jsk = list(
      name = "jsk",
      bg = "#ffdc11",
      fg = "#0c973c",
      mar = c(0, 0, 1.5, 0),
      tab = FALSE,
      pos = "left",
      inner = FALSE,
      line = 1.5,
      cex = 1,
      font = 2
    )
    
  )
  
  
  if (is.list(x)) {
    theme <- x
  } else {
    if (!x %in% names(themes)) {
      stop(paste0(
        "x should be one of ",
        paste0(names(themes), collapse = ", ")
      ),
      call. = FALSE
      )
    } else {
      theme <- themes[[x]]
    }
  }
  
  # print(theme)
  # par(mar = theme[["mar"]])
  .gmapsf$args <- as.list(theme)
  
  return(invisible(theme))
}
