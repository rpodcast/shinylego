# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
#rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ---- 
## This will build a tar.gz that can be installed locally, 
## sent to CRAN, or to a package manager
devtools::build()

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile()

## If you want to deploy to ShinyProxy
golem::add_dockerfile_shinyproxy()

## If you want to deploy to Heroku
golem::add_dockerfile_heroku()

## deploy app
# if deploying in vs code container, must create /workspaces/rsconnect dir first
# sudo mkdir /workspaces/rsconnect
# sudo chown -R eric:eric /workspaces/rsconnect

# one-time operation
# rsconnect::setAccountInfo(
#     name = "rpodcast",
#     token = Sys.getenv("RSCONNECT_TOKEN"),
#     secret = Sys.getenv("RSCONNECT_SECRET")
# )

library(gert)
current_branch <- git_branch()

if (current_branch == "main") {
    rsconnect::deployApp(
        appName = "shinycal", 
        appFileManifest = "dev/app_manifest.txt", 
        launch.browser = FALSE, 
        forceUpdate = TRUE
    )
} else if (current_branch == "dev") {
    rsconnect::deployApp(
        appName = "shinycal_dev", 
        appFileManifest = "dev/app_manifest.txt", 
        launch.browser = FALSE, 
        forceUpdate = TRUE
    )
} else {
    message("Only the dev and main branches are deployable, nothing to do!")
}

