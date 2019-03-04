# Module UI
#' @title   mod_instructionsui and mod_instructions
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @examples 
mod_instructionsui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module server
#' mod_instructions server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_instructionsui
    
mod_instructions <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_instructionsui("m1")
    
## To be copied in the server
# callModule(mod_instructions, "m1")
 
