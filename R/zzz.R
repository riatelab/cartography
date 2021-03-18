.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "This project enters in maintenance mode. 
Core functionalities of `cartography` can be found in `mapsf`.
https://riatelab.github.io/mapsf/"
  )
  packageStartupMessage(msg)
}