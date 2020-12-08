.onAttach <- function(libname, pkgname) {
  msg <- paste0("\nSince version 3.0.0 several replacement function have been introduced.\n", 
                "You can use `tc_legacy_mode('on')` to suppress deprecation warnings.")
  packageStartupMessage(msg)
}
