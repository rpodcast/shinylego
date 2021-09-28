#' process_image UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_process_image_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::actionBttn(
      ns("apply_settings"),
      label = "Create!",
      icon = icon("puzzle-piece"),
      style = "jelly",
      color = "success",
      size = "md"
    )
  )
}
    
#' process_image Server Function
#'
#' @noRd 
mod_process_image_server <- function(input, output, session, image_layer2){
  ns <- session$ns
  
  # establish reactive values
  image_lego <- reactiveVal(NULL)
  
  # reactive for third layer (collect_bricks)
  observeEvent(input$apply_settings, {
    req(image_layer2())
    tictoc::tic("collect_bricks processing")
    res <- brickr:::collect_bricks(
      image_list = image_layer2(),
      use_bricks = NULL,
      default_piece_type = "b"
    )
    tictoc::toc()
    image_lego(res)
  })
  
  image_lego
}
    
## To be copied in the UI
# mod_process_image_ui("process_image_ui_1")
    
## To be copied in the server
# callModule(mod_process_image_server, "process_image_ui_1")
 
