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
        status = "primary",
        "Navbar text"
      ),
      
      # left sidebar
      sidebar = bs4DashSidebar(
        skin = "dark",
        status = "primary",
        title = "ShinyLEGO",
        brandColor = "primary",
        url = "https://r-podcast.org",
        src = "LegoBackground.png",
        elevation = 3,
        opacity = 0.8,
        
        # left sidebar menu
        bs4SidebarMenu(
          bs4SidebarHeader("Header"),
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
              title = "Welcome to the app!",
              lead = "This is the small text appearing right below the title",
              #tagList(
              "This is text that could be any element",
              
              #),
              status = "primary",
              href = "https://r-podcast.org"
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
                  h3("Choose an image for your LEGO mosaic! File formats supported are JPEG, JPG, and PNG."),
                  mod_upload_graphicui("m1")
                )
              )
            ),
            mod_scale_imageui("m2"),
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
              col_12(
                bs4Card(
                  title = "Bricks Required",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  closable = FALSE,
                  labelStatus = "primary",
                  labelText = "",
                  width = 12,
                  #"no way"
                  mod_display_piecesui("inst_pieces")
                )
              )
            ),
            fluidRow(
              col_12(
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
      
      # right sidebar
      controlbar = bs4DashControlbar(
        skin = "dark",
        title = "My right sidebar",
        p("Hello"),
        width = 300
      ),
      
      # footer
      footer = bs4DashFooter(
        copyrights = a(
          href = "https://twitter.com/thercast",
          target = "_blank",
          "@thercast"
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
