# Module UI
#' @title   mod_display_piecesui and mod_display_pieces
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @examples 
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
    
mod_display_pieces <- function(input, output, session, steps_obj, step_choice) {
  ns <- session$ns
  
  # reactive for plot object
  image_obj <- reactive({
    pcs_steps_all <- step_pieces(steps_obj())
    
    # ensure that each step's bricks are only the total for that particular step
    pcs_steps_all <- pcs_steps_all %>%
      group_by(Brick_size, Lego_name, Lego_color) %>%
      mutate(n = n - lag(n, default = 0)) %>%
      ungroup
    
    step_choice_sub <- step_choice()
    display_pieces(pcs_steps_all, step_id = step_choice_sub)
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
 
