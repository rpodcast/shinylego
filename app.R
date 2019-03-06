# To deploy, run: rsconnect::deployApp()

pkgload::load_all()
options( "golem.app.prod" = TRUE)
shiny::shinyApp(ui = app_ui(), server = app_server)
