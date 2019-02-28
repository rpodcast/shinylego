# To deploy, run: rsconnect::deployApp()

pkgload::load_all()
options( "golem.app.prod" = TRUE)
run_app()
