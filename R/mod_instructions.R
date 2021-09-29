# Module UI
#' @title   mod_instructionsui and mod_instructions
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#'  
mod_instructionsui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        bs4Card(
          title = "Instructions Preparation",
          status = "primary",
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          label = NULL,
          width = 12,
          fluidRow(
            col_6(
              sliderInput(
                ns("n_steps"),
                label = "Total Steps",
                min = 1,
                max = 40,
                value = 6,
                step = 1,
                width = "100%"
              )
            ),
            col_3(
              uiOutput(ns("steps_placeholder"))
            )
              # bs4Callout(
              #   title = "Create your mosaic instructions!",
              #   width = 12,
              #   elevation = 1,
              #   status = "success",
              #   "Define the number of steps used in your customized LEGO mosaic instructions. The maximum number of steps is 40."
              # ),
          )
        )
      )
    ),
    fluidRow(
      col_6(
        bs4TabCard(
          id = ns("bricks_req"),
          title = "Bricks Required",
          status = NULL,
          solidHeader = FALSE,
          width = 12,
          side = "right",
          selected = "Diagram",
          tabPanel(
            title = "Diagram",
            mod_display_piecesui(ns("inst_pieces"))
          ),
          tabPanel(
            title = "Table",
            mod_table_piecesui(ns("inst_table"))
          )
        )
      ),
      col_6(
        bs4Card(
          title = "Instructions",
          status = "primary",
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          label = NULL,
          width = 12,
          #"no way"
          mod_display_instructionsui(ns("inst_display"))
        )
      )
    )
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
    
mod_instructions <- function(input, output, session, scale_obj){
  ns <- session$ns
  
  # reactive for steps range
  steps_range <- reactive({
    n_steps <- input$n_steps
    
    step_ids <- purrr::map_chr(1:n_steps, ~paste("Step", (if(.x<10){paste0('0', .x)}else{.x})))
    
    return(
      list(
        n_steps = n_steps,
        steps_int = 1:n_steps,
        step_ids = step_ids
      )
    )
  })
  
  # reactive for steps pieces data frame (long format)
  pcs_steps_df <- reactive({
    req(scale_obj())
    req(steps_range())
    res <- build_pieces_steps_table(
      scale_obj(), 
      num_steps = steps_range()$n_steps, 
      table_format = "long")
    
    return(res)
  })
  
  # render steps selection UI
  output$steps_placeholder <- renderUI({
    req(pcs_steps_df())
    req(steps_range())
    
    shinyWidgets::pickerInput(
      ns("step_choice"),
      "Choose Step",
      choices = steps_range()$step_ids,
      choicesOpt = list(
        subtext = glue::glue("number of bricks: {nb}", nb = dplyr::pull(pcs_steps_df(), n_step))
      )
    )
  })
  
  # reactive for step selected
  step_choice = reactive({
    req(input$step_choice)
    input$step_choice
  })
  
  # display pieces required at selected step
  callModule(
    mod_display_pieces, 
    "inst_pieces", 
    scale_obj = scale_obj, 
    steps_range = steps_range,
    step_choice = step_choice
  )
  
  # show table of required pieces at selected step
  callModule(
    mod_table_pieces, 
    "inst_table",
    scale_obj = scale_obj, 
    steps_range = steps_range,
    step_choice = step_choice
  )
  
  # show instructions at selected step
  callModule(
    mod_display_instructions, 
    "inst_display", 
    scale_obj = scale_obj, 
    steps_range = steps_range,
    step_choice = step_choice
  )
  
}
    
## To be copied in the UI
# mod_instructionsui("m1")
    
## To be copied in the server
# callModule(mod_instructions, "m1")
 
