#' Adds the content of www to www/ from this package
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  options(shiny.maxRequestSize = 30*1024^2)
  shiny::addResourcePath('www', system.file('app/www', package = 'shinylego'))
}
