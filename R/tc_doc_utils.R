
my_params <- function(x) {
  params <- list(
    xfull = "x object of class \\code{sf} or \\code{sfc}",
    x = "x object of class \\code{sf}",
    var = "var name of the variable to plot",
    vars = "var names of the variables to plot",
    type = "type type of map. Values are prop, choro, typo",
    bg = "bg background color",
    border = "border border color",
    lwd = "lwd border width",
    inches = paste0(
      "inches size of the biggest symbol (radius for circles,",
      " half width for squares) in inches."
    ),
    symbol = "symbol type of symbols, 'circle' or 'square'",
    col = "col color",
    leg_pos = paste0(
      "leg_pos position of the legend, one of 'topleft', 'top',",
      "'topright', 'right', 'bottomright', 'bottom', ",
      "'bottomleft', 'left' or a vector of two coordinates ",
      "in map units (c(x, y)). If leg_pos is 'n' then the ",
      "legend is not plotted."
    ),
    leg_title = "leg_title title of the legend",
    leg_title_cex = "leg_title_cex size of the legend title",
    leg_val_cex = "leg_val_cex size of the values in the legend",
    leg_val_rnd = "leg_val_rnd number of decimal places of the values in the legend",
    val_order = "val_order values order, a character vector that matches var modalities",
    leg_frame = "leg_frame whether to add a frame to the legend (TRUE) or not (FALSE)",
    leg_no_data = "leg_no_data label for missing values",
    add = "add whether to add the layer to an existing plot (TRUE) or not (FALSE)",
    pal = paste0(
      "pal a set of colors or a palette name",
      " (from \\link{hcl.colors})"
    ),
    col_na = "col_na color for missing values",
    val_max = "val_max maximum value used for proportional symbols",
    breaks = "breaks either a numeric vector with the actual breaks, or a name of a method a classification method; one of 'fixed', 'sd', 'equal', 'pretty', 'quantile', 'kmeans', 'hclust', 'bclust', 'fisher', 'jenks', 'dpih', 'q6', 'geom', 'arith', 'em' or 'msd' (see Details).",
    nbreaks = "nbreaks number of classes",
    pos = "pos position. It can be one of 'topleft', 'top', 'topright', 'right', 'bottomright', 'bottom', 'bottomleft', 'left' or a vector of two coordinates in map units (c(x, y))"
  )








  for (i in 1:length(params)) {
    params[[i]] <- paste0(
      "@param ", " ",
      params[[i]]
    )
  }

  unname(unlist(params[x]))
}
