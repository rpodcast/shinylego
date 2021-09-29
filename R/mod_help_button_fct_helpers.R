launch_help_modal <- function(current_tab) {
  shiny::showModal(
    shiny::modalDialog(
      title = "More Information",
      size = "l",
      includeMarkdown(app_sys("app", "docs", glue::glue("{current_tab}.md"))),
      easyClose = TRUE
    )
  )
}
