#' @import shiny
#' @import bs4Dash
app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    golem::activate_js(),
    bs4DashPage(
      title = "ShinyLEGO",
      #sidebar_collapsed = FALSE,
      
      # navigation bar
      header = bs4DashNavbar(
        skin = "dark",
        status = "primary"
      ),
      
      # left sidebar
      sidebar = bs4DashSidebar(
        skin = "dark",
        status = "primary",
        title = "ShinyLEGO",
        #brandColor = "primary",
        #src = "LegoBackground.png",
        elevation = 3,
        #opacity = 0.8,
        
        # left sidebar menu
        bs4SidebarMenu(
          #bs4SidebarHeader("Header"),
          bs4SidebarMenuItem(
            "Welcome",
            tabName = "welcome",
            icon = icon('info')
          ),
          bs4SidebarMenuItem(
            "Create",
            tabName = "create",
            icon = icon('palette')
          ),
          bs4SidebarMenuItem(
            "Instructions",
            tabName = "instructions",
            icon = icon('book-open'),
            condition = "output.showinst"
          ),
          bs4SidebarMenuItem(
            "Feedback",
            tabName = "feedback",
            icon = icon('envelope')
          )
        )
      ),
      
      # main body
      body = bs4DashBody(
        shinyjs::useShinyjs(),
        shinyWidgets::chooseSliderSkin("HTML5"),
        bs4TabItems(
          # welcome ui ----
          bs4TabItem(
            tabName = "welcome",
            mod_welcome_ui("welcome_ui_1")
          ),
          
          # create image ----
          bs4TabItem(
            tabName = "create",
            fluidRow(
              col_12(
                bs4Card(
                  title = "Begin your creation!",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  closable = TRUE,
                  label = NULL,
                  width = 12,
                  tagList(
                    p("Choose an image for your LEGO mosaic! File formats supported are JPEG, JPG, and PNG.")
                  )
                )
              )
            ),
            fluidRow(
              col_12(
                mod_mosaic_metricsui("overall_metrics")
              )
            ),
            fluidRow(
              col_12(
                mod_scale_imageui("m2")
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
                  label = NULL,
                  width = 12,
                  tagList(
                    fluidRow(
                      col_12(
                        mod_upload_graphicui("m1"),
                        mod_display_imageui("image")
                      )
                    )
                  )
                  
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
                  label = NULL,
                  width = 12,
                  tagList(
                    fluidRow(
                      col_12(
                        mod_process_image_ui("process_image_ui_1"),
                        mod_display_lego_2dui("m3")
                      )
                    )
                  )
                )
              )
            )
          ),
          # instructions module ----
          bs4TabItem(
            tabName = "instructions",
            mod_instructionsui("mod_instructions_ui_1")
          ),
          bs4TabItem(
            tabName = "feedback",
            mod_feedback_ui("feedback_ui_1")
          )
        )
      ),
      
      # footer
      footer = bs4DashFooter(
        left = a(
          href = "https://r-podcast.org",
          target = "_blank",
          "The R-Podcast"
        ),
        right = "2021"
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
