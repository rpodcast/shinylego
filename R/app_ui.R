#' @import shiny
#' @import bs4Dash
app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    golem::js(),
    bs4DashPage(
      title = "ShinyLEGO",
      sidebar_collapsed = FALSE,
      
      # navigation bar
      navbar = bs4DashNavbar(
        skin = "dark",
        status = "primary"
      ),
      
      # left sidebar
      sidebar = bs4DashSidebar(
        skin = "dark",
        status = "primary",
        title = "ShinyLEGO",
        brandColor = "primary",
        src = "LegoBackground.png",
        elevation = 3,
        opacity = 0.8,
        
        # left sidebar menu
        bs4SidebarMenu(
          #bs4SidebarHeader("Header"),
          bs4SidebarMenuItem(
            "Welcome",
            tabName = "welcome",
            icon = 'info'
          ),
          bs4SidebarMenuItem(
            "Create",
            tabName = "create",
            icon = 'palette'
          ),
          bs4SidebarMenuItem(
            "Instructions",
            tabName = "instructions",
            icon = 'book-open'
          )
        )
      ),
      
      # main body
      body = bs4DashBody(
        shinyjs::useShinyjs(),
        shinyWidgets::chooseSliderSkin("HTML5"),
        bs4TabItems(
          bs4TabItem(
            tabName = "welcome",
            bs4Jumbotron(
              title = "Welcome to the LEGO Mosaic Maker!",
              lead = "This is a Shiny application that lets you convert any picture to a LEGO mosaic directly from the comfort of your web browser!  Once you upload a picture, you can customize many settings.  This app would not be possible without the innovative R scripts created by Ryan Timpe!  Here are links to his excellent blog posts detailing the workflow:",
              list_to_li(
                list(
                  tags$a(href = "http://www.ryantimpe.com/post/lego-mosaic1/", "How To: LEGO mosaics from photos using R & the tidyverse"),
                  tags$a(href = "http://www.ryantimpe.com/post/lego-mosaic2/", "LEGO mosaics: Two weeks later"),
                  tags$a(href = "http://www.ryantimpe.com/post/lego-mosaic3/", "LEGO mosaics: Part 3(D)")
                )
              ),
              status = "primary",
              btn_name = "App GitHub Repository",
              href = "https://gitlab.com/rpodcast/shinylego"
            ),
            fluidRow(
              col_6(
                bs4UserCard(
                  title = "Application Developer",
                  subtitle = "Eric Nantz",
                  status = "info",
                  width = 12,
                  src = "www/pic_with_r_logo_github.jpg",
                  bs4ListGroup(
                    width = 12,
                    bs4ListGroupItem(
                      "The R-Podcast",
                      type = "action",
                      src = "https://r-podcast.org"
                    ),
                    bs4ListGroupItem(
                      "Twitter: @thercast",
                      type = "action",
                      src = "https://twitter.com/thercast"
                    ),
                    bs4ListGroupItem(
                      "Github: rpodcast",
                      type = "action",
                      src = "https://github.com/rpodcast"
                    )
                  )
                )
              ),
              col_6(
                bs4UserCard(
                  title = "Key Contributor",
                  subtitle = "Ryan Timpe",
                  status = "info",
                  width = 12,
                  src = "www/Ryan2018sq.jpg",
                  bs4ListGroup(
                    width = 12,
                    bs4ListGroupItem(
                      "ryantimpe.com",
                      type = "action",
                      src = "http://www.ryantimpe.com/"
                    ),
                    bs4ListGroupItem(
                      "Twitter: @ryantimpe",
                      type = "action",
                      src = "https://twitter.com/ryantimpe"
                    ),
                    bs4ListGroupItem(
                      "GitHub: ryantimpe",
                      type = "action",
                      src = "https://github.com/ryantimpe"
                    )
                  )
                )
              )
            )
          ),
          bs4TabItem(
            tabName = "create", 
            fluidRow(
              col_12(
                bs4Card(
                  title = "Upload your image!",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  closable = FALSE,
                  labelStatus = "primary",
                  labelText = "",
                  width = 12,
                  h4("Choose an image for your LEGO mosaic! File formats supported are JPEG, JPG, and PNG."),
                  p("Note: Conversion process may take some time for pictures with diverse color palettes."),
                  mod_upload_graphicui("m1")
                )
              )
            ),
            mod_scale_imageui("m2"),
            fluidRow(
              col_12(
                mod_mosaic_metricsui("overall_metrics")
              )
            ),
            fluidRow(
              col_6(
                bs4Card(
                  title = "Original Image",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  closable = FALSE,
                  labelStatus = "primary",
                  labelText = "",
                  width = 12,
                  mod_display_imageui("image")
                )
              ),
              col_6(
                bs4Card(
                  title = "LEGO Mosaic!",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  closable = FALSE,
                  labelStatus = "primary",
                  labelText = "",
                  width = 12,
                  mod_display_lego_2dui("m3")
                )
              )
            )
          ),
          bs4TabItem(
            tabName = "instructions",
            fluidRow(
              col_12(
                mod_define_stepsui("inst_steps")
              )
            ),
            fluidRow(
              col_12(
                mod_choose_stepui("choice_steps")
              )
            ),
            fluidRow(
              col_6(
                bs4TabCard(
                  title = "Bricks Required",
                  status = NULL,
                  solidHeader = FALSE,
                  width = 12,
                  side = "right",
                  bs4TabPanel(
                    tabName = "Diagram",
                    active = TRUE,
                    mod_display_piecesui("inst_pieces")
                  ),
                  bs4TabPanel(
                    tabName = "Table",
                    active = FALSE,
                    mod_table_piecesui("inst_table")
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
                  labelStatus = "primary",
                  labelText = "",
                  width = 12,
                  #"no way"
                  mod_display_instructionsui("inst_display")
                )
              )
            )
          )
        )
      ),
      
      # footer
      footer = bs4DashFooter(
        copyrights = a(
          href = "https://r-podcast.org",
          target = "_blank",
          "The R-Podcast"
        ),
        right_text = "2019"
      )
    )
  )
}

golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'shinylego')
  )
  
  tagList(
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
