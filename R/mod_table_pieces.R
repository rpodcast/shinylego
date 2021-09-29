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
    
mod_table_pieces <- function(input, output, session, scale_obj, steps_range, step_choice, width_table = '100%'){
  ns <- session$ns
  
  table_obj <- reactive({
    req(scale_obj())
    req(steps_range())
    req(step_choice())
    
    pcs_new <- build_pieces_steps_table(
      scale_obj(), 
      num_steps = steps_range()$n_steps, 
      table_format = "wide"
    ) %>%
      filter(Step == step_choice()) %>%
      select(-Piece, -Step)
    
    res <- pcs_new %>%
      #mutate(Display = "    ") %>%
      select(`LEGO Brick Color`, Display = Lego_color, everything())
    
    return(res)
  })
  
  output$pieces_table <- DT::renderDT({
    req(table_obj())
    n_cols <- ncol(table_obj())
    DT::datatable(table_obj(), 
                  options = list(
                    dom = 't', 
                    pageLength = 30),
                    #columnDefs = list(list(className = 'dt-center', targets = 0:n_cols))),
                  rownames = FALSE) %>%
      DT::formatStyle(
        'Display',
        'LEGO Brick Color',
        backgroundColor = DT::styleEqual(lego_colors$Color, lego_colors$hex)
      )
    }
  )
}
    
## To be copied in the UI
# mod_table_piecesui("table_piecesui_1")
    
## To be copied in the server
# callModule(mod_table_pieces, "table_piecesui_1")
 
