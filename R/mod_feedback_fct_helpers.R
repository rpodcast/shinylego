#' @import gh
#' @noRd
submit_issue <- function(issue_title, 
                         issue_labels, 
                         issue_description,
                         repo = "rpodcast/shinylego_testing",
                         add_labels = c("from_app")) {
  
  # append name and date to end of issue description
  version <- golem::get_golem_version()
  issue_date <- Sys.Date()
  end_text <- glue::glue("\n\n\n\nFiled from shinyLego version {version} on {issue_date}")
  issue_description <- paste0(issue_description, end_text)
  
  # add the additional labels to the ones provided by the user
  issue_labels <- c(issue_labels, add_labels)
  
  # use the gh package to create the issue on the repo tracker
  issue_res <- gh::gh(
    glue::glue("POST /repos/{repo}/issues"),
    title = issue_title,
    body = issue_description,
    assignee = "rpodcast",
    labels = issue_labels
  )
  
  return(issue_res)
}
