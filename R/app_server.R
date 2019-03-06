#' @import shiny
app_server <- function(input, output, session) {
  
  # upload graphic module
  upload_obj <- callModule(mod_upload_graphic, "m1")
  
  # scale module
  scale_obj <- callModule(mod_scale_image, "m2", upload_obj)
  
  # display 2d lego (all pieces)
  callModule(mod_display_lego_2d, "m3", scale_obj)
}
