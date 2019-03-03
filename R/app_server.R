#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  res <- callModule(mod_upload_graphic, "m1")
}
