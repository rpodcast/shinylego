# Module UI
#' @title   mod_mosaic_metricsui and mod_mosaic_metrics
#' @description  A shiny Module that ...
#'
#' @param id shiny id
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @examples 
mod_mosaic_metricsui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(bs4ValueBoxOutput(ns("total_bricks"), width = NULL)),
      col_3(bs4ValueBoxOutput(ns("unique_brick_sizes"), width = NULL)),
      col_3(bs4ValueBoxOutput(ns("unique_colors"), width = NULL)),
      col_3(bs4ValueBoxOutput(ns("total_cost"), width = NULL))
    )
  )
}
    
# Module server
#' mod_mosaic_metrics server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#' @rdname mod_mosaic_metricsui
    
mod_mosaic_metrics <- function(input, output, session, scale_obj){
  ns <- session$ns
  
  output$total_bricks <- renderbs4ValueBox({
    req(scale_obj())
    total_bricks <- scale_obj()$pieces %>%
      tally() %>%
      pull(n)
    
    bs4ValueBox(
      value = total_bricks,
      subtitle = "Total bricks required",
      status = "info",
      icon = 'toolbox'
    )
  })
  
  output$unique_brick_sizes <- renderbs4ValueBox({
    req(scale_obj())
    unique_bricks <- scale_obj()$pieces %>%
      select(Brick_size) %>%
      distinct() %>%
      pull() %>%
      length()
    
    bs4ValueBox(
      value = unique_bricks,
      subtitle = "Unique brick sizes",
      status = "danger",
      icon = 'shopping-cart'
    )
  })
  
  output$unique_colors <- renderbs4ValueBox({
    req(scale_obj())
    unique_colors <- scale_obj()$pieces %>%
      select(Lego_name) %>%
      distinct() %>%
      pull() %>%
      length()
    
    bs4ValueBox(
      value = unique_colors,
      subtitle = "Unique colors",
      status = "primary",
      icon = 'palette'
    )
  })
  
  output$total_cost <- renderbs4ValueBox({
    req(scale_obj())
    
    # create data frame with total cost for each brick size
    # join with custom price data set
    pieces_cost <- scale_obj()$pieces %>%
      group_by(Brick_size) %>%
      summarize(n_total = sum(n)) %>%
      ungroup %>%
      left_join(
        price_data,
        by = c("Brick_size" = "size")
      ) %>%
      mutate(brick_type_cost = n_total * price)
    
    total_cost <- pieces_cost %>%
      summarize(total_cost = sum(brick_type_cost)) %>%
      pull(total_cost)
    
    bs4ValueBox(
      value = total_cost,
      subtitle = "Estimated Cost (dollars)",
      status = "warning",
      icon = 'file-invoice-dollar'
    )
  })
}
    
## To be copied in the UI
# mod_mosaic_metricsui("mosaic_metricsui_1")
    
## To be copied in the server
# callModule(mod_mosaic_metrics, "mosaic_metricsui_1")
 
