# nested list containing input / tab IDs and more verbose labels

app_key <- list(
  current_tab = list(
    label = NA,
    choices = c(
      "Welcome" = "welcome",
      "Create" = "create",
      "Instructions" = "instructions",
      "Feedback" = "feedback"
    ),
    default = "welcome",
    module = NA
  )
)

usethis::use_data(app_key, overwrite = TRUE)
