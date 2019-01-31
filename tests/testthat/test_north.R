context("Plot North Arrow")

fufun <- function(){
  for (i in list("topleft", "top", "topright", "right", "bottomright",
                 "bottom", "bottomleft", "left")){
    north(i, south = FALSE)
  }
}

test_that("north arrow plot", {
  plot(st_geometry(mtq))
  expect_silent(fufun())
  expect_silent(north(pos =  c(746368, 1632993), south = TRUE))
})
