#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Jumbotron(
      title = "Welcome to the Shiny LEGO Mosaic Maker!",
      lead = "This is a Shiny application that lets you convert any picture to a LEGO mosaic directly from the comfort of your web browser!  Once you upload a picture, you can customize many settings.  This app would not be possible without the brickr package created by Ryan Timpe!",
      status = "info",
      btnName = "App GitHub Repository",
      href = "https://gitlab.com/rpodcast/shinylego"
    ),
    fluidRow(
      col_6(
        bs4UserCard(
          title = bs4UserDescription(
            title = "Application Developer",
            subtitle = "Eric Nantz",
            image = "www/pic_with_r_logo_github.jpg",
            type = 1
          ),
          status = "info",
          width = 12,
          
          bs4ListGroup(
            width = 12,
            type = "action",
            bs4ListGroupItem(
              "The R-Podcast",
              href = "https://r-podcast.org"
            ),
            bs4ListGroupItem(
              "Twitter: @thercast",
              href = "https://twitter.com/thercast"
            ),
            bs4ListGroupItem(
              "Github: rpodcast",
              href = "https://github.com/rpodcast"
            )
          )
        )
      ),
      col_6(
        bs4UserCard(
          title = bs4UserDescription(
            title = "Key Contributor",
            subtitle = "Ryan Timpe",
            image = "www/Ryan2018sq.jpg",
            type = 1
          ),
          status = "info",
          width = 12,
          
          bs4ListGroup(
            width = 12,
            type = "action",
            bs4ListGroupItem(
              "ryantimpe.com",
              href = "http://www.ryantimpe.com/"
            ),
            bs4ListGroupItem(
              "Twitter: @ryantimpe",
              href = "https://twitter.com/ryantimpe"
            ),
            bs4ListGroupItem(
              "GitHub: ryantimpe",
              href = "https://github.com/ryantimpe"
            )
          )
        )
      )
    )
  )
}
    
#' welcome Server Function
#'
#' @noRd 
mod_welcome_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_ui_1")
    
## To be copied in the server
# callModule(mod_welcome_server, "welcome_ui_1")
 
