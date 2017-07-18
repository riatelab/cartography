.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\ncartography uses mostly sf instead of sp and rgeos since verison 2.0.0.")
  packageStartupMessage("A few breaking changes have been introduced, see the NEWS file for details: print(news(package = 'cartography'))")
}
