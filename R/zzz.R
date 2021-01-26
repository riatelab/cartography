.onAttach <- function(libname, pkgname) {
  msg <- paste0("This version introduces functions that supersede previous ones.\n", 
                "Use `tc_legacy_mode('on')` to suppress deprecation warnings.")
  packageStartupMessage(msg)
}
