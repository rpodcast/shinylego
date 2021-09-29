#' @import shiny
app_server <- function(input, output, session) {
  
  # reactive for current tab viewed in the interface
  current_tab <- reactive({
    input$current_tab
  })
  
  # help button module
  callModule(mod_help_button_server, "help_button_ui_1", current_tab)
  
  # upload graphic module
  upload_obj <- callModule(mod_upload_graphic, "m1")
  
  # display original image module
  callModule(mod_display_image, "image", upload_obj)

  # scale module
  scale_layer2 <- callModule(mod_scale_image, "m2", upload_obj)
  scale_obj <- callModule(mod_process_image_server, "process_image_ui_1", scale_layer2)
  
  # display overall metrics
  callModule(mod_mosaic_metrics, "overall_metrics", scale_obj)
  
  # display 2d lego (all pieces)
  callModule(mod_display_lego_2d, "m3", scale_obj)
  
  # condition for showing instructions tab
  output$showinst <- reactive({
    !is.null(scale_obj())
  })
  
  # instructions module
  callModule(mod_instructions, "mod_instructions_ui_1", scale_obj)
  
  # feedback module
  callModule(mod_feedback_server, "feedback_ui_1")
  
  # execute conditional ui server side ----
  output_names <- c("showinst")
  purrr::walk(output_names, ~outputOptions(output, .x, suspendWhenHidden = FALSE))
}

