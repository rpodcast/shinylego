# Module UI
#' @title   mod_scale_imageui and mod_scale_image
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @import shinyWidgets
#' @import shinycustomloader
#' @examples 
mod_scale_imageui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      div(
        id = ns("lego_controls"),
        bs4Card(
          title = "Customize LEGO Mosiac Settings",
          status = "primary",
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = FALSE,
          closable = FALSE,
          labelStatus = "primary",
          labelText = "",
          width = 12,
          tagList(
            fluidRow(
              col_3(
                shinyWidgets::prettyRadioButtons(
                  ns("shape"),
                  "Shape",
                  choices = c(
                    "Square",
                    "Rectangle"
                  ),
                  inline = TRUE,
                  status = "primary",
                  fill = TRUE,
                  animation = "smooth" 
                )
              ),
              col_2(
                uiOutput(ns("dimension_placeholder"))
              ),
              col_3(
                sliderInput(
                  ns("brightness"),
                  "Brightness",
                  min = 0,
                  max = 10,
                  value = 1,
                  step = 1
                )
              ),
              col_3(
                shinyWidgets::radioGroupButtons(
                  ns("theme"),
                  label = "Theme",
                  choiceNames = c(
                    "Color",
                    "Black & White"
                  ),
                  choiceValues = c(
                    "default",
                    "bw"
                  ),
                  justified = TRUE,
                  status = "primary",
                  checkIcon = list(
                    yes = icon("ok", lib = "glyphicon")
                  )
                )
              )
            )
            # fluidRow(
            #   col_12(
            #     withLoader(plotOutput(ns("mosaic_2d")), type = "image", loader = "lego_loader.gif")
            #   )
            # )
          )
        )
      )
    )
  )
}
    
# Module server
#' mod_scale_image server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_scale_imageui
    
mod_scale_image <- function(input, output, session, img_processed){
  ns <- session$ns
  
  # show lego image controls when img_processed is valid
  observeEvent(img_processed()$image_obj, {
    if (!isTruthy(img_processed()$image_obj)) {
      return(NULL)
    } else {
      shinyjs::show('lego_controls')
    }
  })
  # dynamic output for shape dimensions
  # if "square": display only one number
  # if "rectangle": display separate inputs for width and height
  output$dimension_placeholder <- renderUI({
    ns <- session$ns
    
    if (input$shape == "Square") {
      ui <- numericInput(
              ns("dimension_width"),
              "Plate size",
              value = 48,
              min = 12,
              max = 1000,
              step = 1,
            )
    } else {
      ui <- tagList(
        fluidRow(
          col_6(
            numericInput(
              ns("dimension_width"),
              "Plate width",
              value = 48,
              min = 12,
              max = 1000,
              step = 1,
            )
          ),
          col_6(
            numericInput(
              ns("dimension_height"),
              "Plate height",
              value = 48,
              min = 12,
              max = 1000,
              step = 1,
            )
          )
        )
      )
    }
    
    return(ui)
  })
  
  # reactive for dimension vector
  dimension_obj <- reactive({
    req(input$dimension_width)
    if (input$shape == "Square") {
      res <- input$dimension_width
    } else {
      res <- c(input$dimension_width, input$dimension_height)
    }
    
    return(res)
  })
  
  # reactive object for lego version of image
  image_lego <- reactive({
    req(img_processed())
    req(dimension_obj())
    res <- scale_image(image = img_processed()$image_obj,
                img_size = dimension_obj(),
                brightness = input$brightness,
                warhol = 1:3) %>%
      legoize(theme = input$theme, contrast = 1) %>%
      collect_bricks(mosaic_type = "flat")
    
    return(res)
  })
  
  # # reactive for plot object
  # image_obj <- reactive({
  #   display_set(image_lego(), title = NULL)
  # })
  # 
  # # display 2d lego plot
  # output$mosaic_2d <- renderPlot({
  #   print(image_obj())
  # })
  
  # return objects
  image_lego
}
    
## To be copied in the UI
# mod_scale_imageui("m1")
    
## To be copied in the server
# callModule(mod_scale_image, "m1")
 
