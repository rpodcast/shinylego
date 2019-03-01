#' @import shiny
#' @import bs4Dash
app_ui <- function() {
  bs4DashPage(
    title = "ShinyLEGO",
    sidebar_collapsed = FALSE,
    
    # navigation bar
    navbar = bs4DashNavbar(
      skin = "dark",
      status = "primary",
      "Navbar text",
      fixed = TRUE
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
        )
      )
    ),
    
    # main body
    body = bs4DashBody(
      bs4TabItem(
        tabName = "welcome",
        bs4Jumbotron(
          title = "Welcome to the app!",
          lead = "This is the small text appearing right below the title",
          "This is text that could be any element",
          status = "primary",
          href = "https://r-podcast.org"
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
}
