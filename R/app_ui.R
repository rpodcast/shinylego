#' @import shiny
#' @import bs4Dash
app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    golem::js(),
    bs4DashPage(
      title = "ShinyLEGO",
      sidebar_collapsed = TRUE,
      
      # navigation bar
      navbar = bs4DashNavbar(
        skin = "dark",
        status = "primary",
        "Navbar text"
        #fixed = TRUE
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
        bs4TabItems(
          bs4TabItem(
            tabName = "welcome",
            bs4Jumbotron(
              title = "Welcome to the app!",
              lead = "This is the small text appearing right below the title",
              #tagList(
              "This is text that could be any element",
              mod_upload_graphicui("m1"),
              #),
              status = "primary",
              href = "https://r-podcast.org"
            )
          ),
          bs4TabItem(
            tabName = "create", 
            mod_scale_imageui("m2"),
            fluidRow(
              col_6(
                mod_display_lego_2dui("m3")
              )
              # col_6(
              #   mod_display_lego_3dui("m4")
              # )
            )
          ),
          bs4TabItem(
            tabName = "instructions"
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
