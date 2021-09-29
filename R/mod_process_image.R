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
    ),
    div(id = ns("alert_anchor"))
  )
}
    
#' process_image Server Function
#'
#' @noRd 
mod_process_image_server <- function(input, output, session, image_layer2){
  ns <- session$ns
  
  # establish reactive values
  image_lego <- reactiveVal(NULL)
  
  # assemble bricks
  observeEvent(input$apply_settings, {
    message("alert popped up")
    bs4Dash::createAlert(
      id = "alert_anchor",
      options = list(
        status = "info",
        closable = TRUE,
        width = 12,
        content = "Assembling bricks for mosaic. Please stand by..."
      )
    )
    req(image_layer2())
    #tictoc::tic("collect_bricks processing")
    res <- brickr:::collect_bricks(
      image_list = image_layer2(),
      use_bricks = NULL,
      default_piece_type = "b"
    )
    bs4Dash::closeAlert(id = "alert_anchor")
    #tictoc::toc()
    image_lego(res)
  })
  
  observeEvent(input$alert_anchor, {
    if (!input$alert_anchor) {
      bs4Dash::toast(
        title = "LEGO mosaic is ready!",
        options = list(
          class = "bg-lime",
          autohide = TRUE,
          delay = 2000,
          position = "topRight"
        )
      )
    }
  })
  
  image_lego
}
    
## To be copied in the UI
# mod_process_image_ui("process_image_ui_1")
    
## To be copied in the server
# callModule(mod_process_image_server, "process_image_ui_1")
 
