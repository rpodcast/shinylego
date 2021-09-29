#' help_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_help_button_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::actionButton(
      ns("help_launch"),
      "Info",
      icon = icon("info-circle"),
      style = "color: white; background: green;"
    )
  )
}

#' help_button Server Function
#'
#' @noRd 
mod_help_button_server <- function(input, output, session, current_tab){
  ns <- session$ns
  
  observeEvent(current_tab(), {
    message(glue::glue("the current tab is {t}", t = current_tab()))
    tab_choices <- app_key[["current_tab"]][["choices"]]
    new_label <- paste(names(tab_choices[tab_choices == current_tab()]), "Guide")
    
    updateActionButton(
      session,
      inputId = "help_launch",
      label = new_label
    )
  })
  
  observeEvent(input$help_launch, launch_help_modal(current_tab()))
}

## To be copied in the UI
# mod_help_button_ui("help_button_ui_1")

## To be copied in the server
# callModule(mod_help_button_server, "help_button_ui_1")

