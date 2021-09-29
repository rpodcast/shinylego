#' feedback UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_feedback_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        bs4Card(
          title = "Feedback",
          status = "primary",
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = TRUE,
          label = NULL,
          width = 12,
          tagList(
            includeMarkdown(app_sys("app", "docs", "feedback_intro.md"))
          )
        ),
        bs4Card(
          title = "Details",
          status = "primary",
          solidHeader = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          closable = FALSE,
          label = NULL,
          width = 12,
          tagList(
            fluidRow(
              col_6(
                textInput(
                  ns("issue_title"),
                  with_red_star("Title"),
                  value = "",
                  placeholder = "enter short label",
                  width = "100%"
                )
              ),
              col_6(
                shinyWidgets::pickerInput(
                  ns("issue_labels"),
                  label = with_red_star("Choose one category"),
                  choices = c("bug", "enhancement"),
                  multiple = FALSE
                )
              )
            ),
            fluidRow(
              col_12(
                textAreaInput(
                  ns("issue_description"),
                  label = with_red_star("Enter description"),
                  value = "",
                  width = "300%",
                  cols = 80,
                  rows = 10,
                  placeholder = "markdown format permitted",
                  resize = "vertical"
                )
              )
            ),
            fluidRow(
              col_2(
                shinyWidgets::actionBttn(
                  ns("submit_issue"),
                  "Submit!",
                  icon = icon("save"),
                  style = "jelly",
                  color = "success",
                  size = "sm"
                )
              )
            )
          )
        )
      )
    )
  )
}
    
#' feedback Server Function
#'
#' @noRd 
mod_feedback_server <- function(input, output, session){
  ns <- session$ns
  
  observeEvent(input$submit_issue, {
    # perform checks for mandatory inputs
    if (!shiny::isTruthy(input$issue_title)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "Please enter a title for your issue.",
        type = "error"
      )
      return(NULL)
    }
    
    if (!shiny::isTruthy(input$issue_labels)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "Please choose at least one category for your issue.",
        type = "error"
      )
      return(NULL)
    }
    
    if (!shiny::isTruthy(input$issue_description)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Oops!",
        text = "Please enter a description for your issue.",
        type = "error"
      )
      return(NULL)
    }
    
    # submit issue
    res <- submit_issue(
      input$issue_title, 
      input$issue_labels, 
      input$issue_description,
      repo = get_golem_config("feedback_repo")
    )
    
    # show confirmation
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Issue Submitted!",
      text = "Thank you for your feedback! Your issue has been submitted to the shinylego issue tracker. One of the maintainers will review your issue and contact you for additional details.",
      type = "success"
    )
    
    # reset key inputs
    updateTextInput(
      session,
      "issue_title",
      value = ""
    )
    
    updateTextAreaInput(
      session,
      "issue_description",
      value = ""
    )
    
    shinyWidgets::updatePickerInput(
      session,
      "issue_labels",
      selected = character(0)
    )
  })

 
}
    
## To be copied in the UI
# mod_feedback_ui("feedback_ui_1")
    
## To be copied in the server
# callModule(mod_feedback_server, "feedback_ui_1")
 
