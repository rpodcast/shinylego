# Module UI
#' @title   mod_display_lego_3dui and mod_display_lego_3d
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @examples 
mod_display_lego_3dui <- function(id){
  ns <- NS(id)
  tagList(
    withLoader(rglwidgetOutput(ns("mosaic_3d")), type = "image", loader = "lego_loader.gif")
  )
}
    
# Module server
#' mod_display_lego_3d server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @import rgl
#' @import rayshader
#' @export
#' @rdname mod_display_lego_3dui
    
mod_display_lego_3d <- function(input, output, session, image_lego_obj){
  ns <- session$ns
  
  output$mosaic_3d <- renderRglwidget({
    req(image_lego_obj())
    
    obj_3d <- collect_3d(image_lego_obj()) %>%
      display_3d(.)
    
    rayshader::render_snapshot()
    rgl::rglwidget()
  })
}
    
## To be copied in the UI
# mod_display_lego_3dui("m1")
    
## To be copied in the server
# callModule(mod_display_lego_3d, "m1")
 
