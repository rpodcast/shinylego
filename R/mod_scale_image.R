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
#' @importFrom brickr image_to_mosaic build_mosaic
#'  
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
                numericInput(
                  ns("dimension_width"),
                  "Plate width",
                  value = 48,
                  min = 12,
                  max = 1000,
                  step = 1,
                ),
                numericInput(
                  ns("dimension_height"),
                  "Plate height",
                  value = 48,
                  min = 12,
                  max = 1000,
                  step = 1,
                )
              ),
              col_3(
                sliderInput(
                  ns("brightness"),
                  "Brightness",
                  min = 0,
                  max = 10,
                  value = 1,
                  step = 0.1
                )
              ),
              col_3(
                shinyWidgets::radioGroupButtons(
                  ns("color_category"),
                  label = "Color Palette",
                  status = "primary",
                  choices = c(
                    "Defaults" = "default",
                    #"Customized" = "custom",
                    "Black & White" = "bw"
                  ),
                  checkIcon = list(
                    yes = icon("check-square")
                  )
                ),
                conditionalPanel(
                  condition = "input.color_category == 'default'",
                  ns = ns,
                  shinyWidgets::pickerInput(
                    ns("theme"),
                    label = "Theme",
                    choices = c(
                      "Universal" = "universal",
                      "Generic" = "generic",
                      "Special" = "special"
                      #"Black & White" = "bw"
                    ),
                    selected = c("universal", "generic", "special"),
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.color_category == 'custom'",
                  ns = ns,
                  shinyWidgets::actionBttn(
                    ns("launch_color_table"),
                    "Show Table",
                    style = "bordered",
                    color = "success",
                    icon = icon("paint-roller")
                  )
                ),
                conditionalPanel(
                  condition = "input.color_category == 'bw'",
                  ns = ns,
                  sliderInput(
                    ns("contrast"),
                    "Contrast",
                    min = 0,
                    max = 10,
                    value = 1,
                    step = 0.1
                  )
                )
              ),
              col_2(
                shinyWidgets::pickerInput(
                  ns("match_alg"),
                  "Color match algorithm",
                  choices = list(
                    euclidean = c("euclidean"),
                    cielab = c("cie1976", "cie94", "cie2000", "cmc")
                  ),
                  selected = "euclidean"
                )
              )
              # col_3(
              #   colourpicker::colourInput(
              #     ns("warhol_color"),
              #     label = "Warhol Color",
              #     value = "#010203"
              #   )
              # )
            )
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
      message("lego controls should work now")
      shinyjs::show('lego_controls')
    }
  })
  
  # reactive for dimension vector
  dimension_obj <- reactive({
    req(input$dimension_width)
    if (input$dimension_width == input$dimension_height) {
      res <- input$dimension_width
    } else {
      res <- c(input$dimension_width, input$dimension_height)
    }
    
    return(res)
  })
  
  # reactive for warhol color
  # warhol_obj <- reactive({
  #   req(input$warhol_color)
  #   as.vector(col2rgb(input$warhol_color))
  # })
  
  # reactive for color palette options
  palette_list <- reactive({
    req(input$color_category)
    #custom_df <- brickr::lego_colors
    if (input$color_category == "default") {
      res <- list(
        color_palette = input$theme,
        #color_table = custom_df,
        contrast = 1
      )
    } else if (input$color_category == "custom") {
      custom_df <- brickr::lego_colors
      res <- list(
        color_palette = c("universal", "generic", "special"),
        #color_table = custom_df,
        contrast = 1
      )
    } else if (input$color_category == "bw") {
      res <- list(
        color_palette = "bw",
        #color_table = NULL,
        contrast = input$contrast
      )
    } else {
      stop("color category selected is not supported", call. = FALSE)
    }
    
    return(res)
  })
  
  # reactive for first layer (image_to_scaled)
  image_layer1 <- reactive({
    req(img_processed())
    req(dimension_obj())
    req(input$brightness)
    
    if (is.null(input$theme)) {
      shinyWidgets::show_alert(
        title = "Heads up!",
        text = "Please select at least one color theme.",
        type = "error",
        btn_labels = "Ok"
      )
      return(NULL)
    }
    
    #tictoc::tic("image_to_scaled processing")
    res <- brickr:::image_to_scaled(
      image = img_processed()$image_obj,
      img_size = dimension_obj(),
      brightness = input$brightness,
      warhol = 1:3
    )
    #tictoc::toc()
    return(res)
  })
  
  # reactive for second layer (scaled_to_colors)
  image_layer2 <- reactive({
    req(image_layer1())
    req(palette_list())
    req(input$match_alg)
    #tictoc::tic("scaled_to_colors processing")
    res <- brickr:::scaled_to_colors(
      image_list = image_layer1(),
      color_palette = palette_list()$color_palette,
      #color_table = palette_list()$color_table,
      trans_bg = "White",
      dithering = FALSE,
      contrast = palette_list()$contrast,
      method = input$match_alg,
      default_piece_type = "b"
    )
    #tictoc::toc()
    
    return(res)
  })
  
  # return objects
  image_layer2
}
    
## To be copied in the UI
# mod_scale_imageui("m2")
    
## To be copied in the server
# callModule(mod_scale_image, "m2")
 
