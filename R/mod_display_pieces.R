# Module UI
#' @title   mod_display_piecesui and mod_display_pieces
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#'  
mod_display_piecesui <- function(id, height_window = 500) {
  ns <- NS(id)
  tagList(
    withLoader(
      plotOutput(
        ns("pieces_plot"),
        height = paste0(height_window, 'px')
      ), 
      type = "image", 
      loader = "www/lego_loader.gif")
  )
}
    
# Module server
#' mod_display_pieces server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_display_piecesui
    
mod_display_pieces <- function(input, output, session, scale_obj, steps_range, step_choice) {
  ns <- session$ns
  
  # reactive for plot object
  image_obj <- reactive({
    req(scale_obj())
    req(steps_range())
    req(step_choice())
    
    pcs_steps_all <- build_pieces_steps_table(
      scale_obj(), 
      num_steps = steps_range()$n_steps, 
      table_format = "long"
    )
    
    build_pieces_steps(pcs_steps_all, step = step_choice())
  })
  
  # display 2d lego plot
  output$pieces_plot <- renderPlot({
    print(image_obj())
  })
  
}
    
## To be copied in the UI
# mod_display_piecesui("m1")
    
## To be copied in the server
# callModule(mod_display_pieces, "m1")
 
