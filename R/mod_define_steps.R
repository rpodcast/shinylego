# Module UI
#' @title   mod_define_stepsui and mod_define_steps
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @examples 
mod_define_stepsui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Card(
      title = "Instructions Settings",
      status = "primary",
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = FALSE,
      closable = FALSE,
      labelStatus = "primary",
      labelText = "",
      width = 12,
      fluidRow(
        col_12(
          bs4Callout(
            title = "Create your mosaic instructions!",
            width = 12,
            elevation = 1,
            status = "success",
            "Define the number of steps used in your customized LEGO mosaic instructions. The maximum number of steps is 40."
          ),
          sliderInput(
            ns("n_steps"),
            label = NULL,
            min = 1,
            max = 40,
            value = 6,
            step = 1,
            width = "100%"
          )
        )
      )
    )
  )
}
    
# Module server
#' mod_define_steps server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_define_stepsui
    
mod_define_steps <- function(input, output, session){
  ns <- session$ns
  
  # define return object (number of steps and actual step IDs)
  res <- reactive({
    n_steps <- input$n_steps
    
    step_ids <- map_chr(1:n_steps, ~paste("Step", (if(.x<10){paste0('0', .x)}else{.x})))
    
    return(
      list(
        n_steps = n_steps,
        step_ids = step_ids
      )
    )
  })
  
  res
}
    
## To be copied in the UI
# mod_define_stepsui("m1")
    
## To be copied in the server
# callModule(mod_define_steps, "m1")
 
