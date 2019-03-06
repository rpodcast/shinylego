#' Adds the content of www to www/ from this package
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  op <- options()
  op.shinylego <- list(
    shiny.maxRequestSize = 30*1024^2
  )
  toset <- !(names(op.shinylego) %in% names(op))
  
  if(any(toset)) options(op.shinylego[toset])
  
  shiny::addResourcePath('www', system.file('app/www', package = 'shinylego'))
}
