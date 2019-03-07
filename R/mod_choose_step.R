# Module UI
#' @title   mod_choose_stepui and mod_choose_step
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @examples 
mod_choose_stepui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("steps_placeholder"))
  )
}
    
# Module server
#' mod_choose_step server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_choose_stepui
    
mod_choose_step <- function(input, output, session, steps_obj, steps_inst_obj){
  ns <- session$ns
  
  # reactive for total number of bricks at each step
  bricks_steps <- reactive({
    req(steps_obj())
    pcs_steps_all <- step_pieces(steps_obj()) 
    
    pcs_steps_all %>%
      group_by(Step) %>%
      summarize(cumulative_total = sum(n)) %>%
      mutate(step_total = cumulative_total - lag(cumulative_total, default = 0)) %>%
      ungroup
  })
  
  output$steps_placeholder <- renderUI({
    req(steps_inst_obj())
    shinyWidgets::pickerInput(
      ns("step_choice"),
      "Choose Step",
      choices = steps_inst_obj()$step_ids,
      choicesOpt = list(
        subtext = paste("n(bricks)", pull(bricks_steps(), step_total), sep = ": ")
      )
    )
  })
  
  # assemble return object
  res <- reactive({
    input$step_choice
  })
  
  res
}
    
## To be copied in the UI
# mod_choose_stepui("m1")
    
## To be copied in the server
# callModule(mod_choose_step, "m1")
 
