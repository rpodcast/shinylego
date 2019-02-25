# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 
# 1 - On init
# 
## 1.1 - Fill the descripion
## 
## Add information about the package that will contain your app

golem::fill_desc(
  pkg_name = "shinylego", # The Name of the package containing the App 
  pkg_title = "Shiny application for creating LEGO mosiacs", # The Title of the package containing the App 
  pkg_description = "Shinylego encapsulates the workflow created by Ryan Timpe to create LEGO mosaics directly with R code based on a single image.", # The Description of the package containing the App 
  author_first_name = "Eric", # Your First Name
  author_last_name = "Nantz",  # Your Last Name
  author_email = "thercast@gmail.com",      # Your Email
  repo_url = "https://github.com/rpodcast/shinylego")      # The (optional) URL of the GitHub Repo

## 1.2 - Set common Files 
## 
## If you want to use the MIT licence, README, code of conduct, lifecycle badge, and news

usethis::use_mit_license(name = "Eric Nantz")  # You can set another licence here
usethis::use_readme_rmd()
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md()

## 1.3 - Add a data-raw folder
## 
## If you have data in your package
usethis::use_data_raw()

## 1.4 - Init Tests
## 
## Create a template for tests

golem::use_recommended_tests()

## 1.5 : Use Recommended Package

golem::use_recommended_dep()

## 1. Add various tools

golem::use_utils_ui()
golem::use_utils_server()
golem::use_utils_prod()
golem::use_favicon()
golem::use_recommended_js()

# The JS functions can also be put inside the app with 

golem::js()

## 1.6 : Create your first module
## 
## Add a module file in your R folder with the recommended structure

golem::add_module(name = "my_first_module") #

## 1.7: Add a browser button

golem::add_browser_button()

# 2. All along your project

## 2.1 Add modules

golem::add_module( name = "my_other_module") # Name of the module 

## 2.2 Add dependencies

usethis::use_package("dplyr")
usethis::use_package("tidyr") 
usethis::use_package("purrr")
usethis::use_package("jpeg")
usethis::use_package("readr")
usethis::use_package("ggplot2")
usethis::use_package("magrittr")
usethis::use_dev_package("shinipsum")

## 2.3 Various tools along the way

golem::add_browser_button()

## 2.4 Add tests

usethis::use_test("app")

# 3. Documentation

## 3. Vignette
usethis::use_vignette("shinylego")
devtools::build_vignettes()

## 3. Code coverage
usethis::use_travis()
usethis::use_appveyor()
usethis::use_coverage()

# 4. Test my package

devtools::test()
rhub::check_for_cran()

# 5. Deployment elements

golem::add_rconnect_file()


