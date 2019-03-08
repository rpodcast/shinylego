# Module UI
#' @title   mod_display_lego_2dui and mod_display_lego_2d
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @examples 
mod_display_lego_2dui <- function(id, height_window = 500){
  ns <- NS(id)
  tagList(
    withLoader(
      plotOutput(
        ns("mosaic_2d"),
        height = paste0(height_window, 'px')
      ), 
      type = "image", 
      loader = "www/lego_loader.gif")
  )
}
    
# Module server
#' mod_display_lego_2d server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_display_lego_2dui
    
mod_display_lego_2d <- function(input, output, session, image_lego_obj){
  ns <- session$ns
  
  # reactive for plot object
  image_obj <- reactive({
    display_set(image_lego_obj(), title = NULL)
  })
  
  # display 2d lego plot
  output$mosaic_2d <- renderPlot({
    print(image_obj())
  })
}
    
## To be copied in the UI
# mod_display_lego_2dui("m1")
    
## To be copied in the server
# callModule(mod_display_lego_2d, "m1")
 
