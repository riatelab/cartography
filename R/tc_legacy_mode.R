#' @title Turn on or off warnings from deprecated functions
#' @description 
#' Use this function to turn off warnings from deprecated function (see Note).
#' The best way to avoid these warnings is to use the suggested replacement
#' function. 
#'
#' @param mode "on" hide deprecation warnings, "off" to show deprecation warnings.    
#' @note Be aware that this function may affect other package output. This is 
#' a wrapper around \code{options(lifecycle_verbosity="quiet")} when 
#' \code{mode="on"} and \code{options(lifecycle_verbosity="default")} when 
#' \code{mode="off"}.
#' 
#' 
#' @export
#'
#' @examples
#' tc_legacy_mode("on")
#' tc_legacy_mode("off")
tc_legacy_mode <- function(mode = "on"){
  if(mode == "on"){
    options(lifecycle_verbosity="quiet")
  }
  if (mode == "off"){
    options(lifecycle_verbosity="default")
  }
}