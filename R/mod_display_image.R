# Module UI
#' @title   mod_display_imageui and mod_display_image
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @examples 
mod_display_imageui <- function(id){
  ns <- NS(id)
  tagList(
    imageOutput(ns("image_render"))
  )
}
    
# Module server
#' mod_display_image server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_display_imageui
    
mod_display_image <- function(input, output, session, img_processed, width = 400, height = 400){
  ns <- session$ns
  
  # display image
  output$image_render <- renderImage({
    list(
      src = img_processed()$image_path,
      width = width,
      height = height
    )
  }, deleteFile = FALSE)
}

    
## To be copied in the UI
# mod_display_imageui("m1")
    
## To be copied in the server
# callModule(mod_display_image, "m1")
 
