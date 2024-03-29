# Module UI
#' @title   mod_display_instructionsui and mod_display_instructions
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#'  
mod_display_instructionsui <- function(id, height_window = 500){
  ns <- NS(id)
  tagList(
    withLoader(
      plotOutput(
        ns("instructions_plot"),
        height = paste0(height_window, 'px')
      ), 
      type = "image", 
      loader = "www/lego_loader.gif")
  )
}
    
# Module server
#' mod_display_instructions server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_display_instructionsui
    
mod_display_instructions <- function(input, output, session, scale_obj, steps_range, step_choice){
  ns <- session$ns
  
  # reactive for plot object
  image_obj <- reactive({
    req(scale_obj())
    req(steps_range())
    req(step_choice())
    
    build_instructions_steps(
      scale_obj(), 
      steps_range()$n_steps, 
      step = step_choice()
    )
  })
  
  # display plot
  output$instructions_plot <- renderPlot({
    print(image_obj())
  })
}
    
## To be copied in the UI
# mod_display_instructionsui("m1")
    
## To be copied in the server
# callModule(mod_display_instructions, "m1")
 
