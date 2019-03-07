# Module UI
#' @title   mod_table_piecesui and mod_table_pieces
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @import DT
#' @examples 
mod_table_piecesui <- function(id){
  ns <- NS(id)
  tagList(
    withLoader(
      DT::DTOutput(
        ns("pieces_table")
      ), 
      type = "image", 
      loader = "www/lego_loader.gif")
  )
}
    
# Module server
#' mod_table_pieces server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @import DT
#' @export
#' @rdname mod_table_piecesui
    
mod_table_pieces <- function(input, output, session, steps_obj, step_choice, width_table = '100%'){
  ns <- session$ns
  
  table_obj <- reactive({
    pcs_steps_all <- step_pieces(steps_obj())
    
    step_choice_sub <- step_choice()
    res <- table_pieces(pcs_steps_all, step_choice_sub)
    
    # remove any rows with all 0 values
    res <- filter_at(res, vars(contains('x')), any_vars(. > 0))
    
    # add a blank column to hold the color styling
    res <- res %>%
      mutate(Display = "    ") %>%
      select(`LEGO Brick Color`, Display, everything())
    
    return(res)
  })
  
  output$pieces_table <- DT::renderDT({
    DT::datatable(table_obj(), 
                  options = list(
                    dom = 't', 
                    pageLenth = 30,
                    columnDefs = list(list(className = 'dt-center', targets = 0:7))),
                  rownames = FALSE) %>%
      DT::formatStyle(
        'Display',
        'LEGO Brick Color',
        backgroundColor = DT::styleEqual(lego_colors$Color, lego_colors$hex_code)
      )
    }
  )
}
    
## To be copied in the UI
# mod_table_piecesui("table_piecesui_1")
    
## To be copied in the server
# callModule(mod_table_pieces, "table_piecesui_1")
 
