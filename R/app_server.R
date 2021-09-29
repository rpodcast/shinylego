#' @import shiny
app_server <- function(input, output, session) {
  
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
  
  # instructions module
  callModule(mod_instructions, "mod_instructions_ui_1", scale_obj)
  
  # number of steps in instructions tab
  #steps_inst_obj <- callModule(mod_define_steps, "inst_steps")
  
  # # reactive for tidy steps data frame
  # lego_steps <- reactive({
  #   generate_steps(scale_obj(), num_steps = steps_inst_obj()$n_steps)
  # })
  
  # step choices
  #step_choice <- callModule(mod_choose_step, "choice_steps", lego_steps, steps_inst_obj)
  
  # display pieces required at selected step
  #callModule(mod_display_pieces, "inst_pieces", scale_obj, steps_inst_obj, step_choice)
  
  # display table version of pieces at selected step
  #callModule(mod_table_pieces, "inst_table", scale_obj, steps_inst_obj, step_choice)
  
  # display instructions  at selected step
  #callModule(mod_display_instructions, "inst_display", scale_obj, steps_inst_obj, step_choice)
}

