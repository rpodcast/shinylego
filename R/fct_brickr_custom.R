build_pieces_steps_table <- function(brick_obj, num_steps = 10, table_format = c("long", "wide")) {
  in_list <- brick_obj
  image <- in_list$Img_bricks
  type <- in_list$brickr_object

  num_steps <- min(abs(round(num_steps)), 40)
  rows_per_step <- ceiling((max(image$ymax)-0.5) / (num_steps+1))
  
  if (type == "mosaic") {
    create_steps <- function(a, n_steps) {
      if(a < n_steps){
        image %>% 
          dplyr::group_by(brick_name) %>% 
          dplyr::filter(min(ymin) <= a*rows_per_step+(min(image$ymin)+0.5)) %>% 
          dplyr::ungroup() %>%
          dplyr::mutate(Step = paste("Step", stringr::str_pad(a, 2, pad = "0")))
      } else {
        image %>% 
          dplyr::mutate(Step = paste("Step", stringr::str_pad(a, 2, pad = "0")))
      }
    }
  }
  
  steps_for_table <- 1:num_steps %>% 
    purrr::map_df(create_steps, num_steps) %>% 
    dplyr::mutate(alpha = 1)
  
  pcs_step_df <- steps_for_table %>%
    mutate(size1 = brick_width, size2 = brick_height) %>%
    mutate(Brick_size = ifelse(size1 > size2,
                               paste(size1, "x", size2),
                               paste(size2, "x", size1))) %>%
    select(Step, Brick_size, Lego_color, Lego_name, Piece = piece_type) %>%
    group_by(Step, Brick_size, Lego_name, Lego_color, Piece) %>%
    summarize(n_cumulative = n()) %>%
    ungroup() %>%
    group_by(Brick_size, Lego_color, Lego_name) %>%
    mutate(n_step = n_cumulative - lag(n_cumulative)) %>%
    ungroup() %>%
    mutate(n_step = ifelse(is.na(n_step), n_cumulative, n_step))
  
  if (table_format == "wide") {
    pcs_step_df <- pcs_step_df %>% 
      dplyr::select(-n_cumulative) %>% 
      tidyr::pivot_wider(names_from = Brick_size, values_from = n_step, values_fill = 0) %>%
      dplyr::rename(`LEGO Brick Color` = Lego_name) %>%
      dplyr::filter_at(dplyr::vars(contains('x')), dplyr::any_vars(. > 0))
  }
  
  return(pcs_step_df)
}

build_instructions_steps <- function(brick_obj, num_steps = 10, step = "Step 01", coord_ratio = 1) {
  in_list <- brick_obj
  image <- in_list$Img_bricks
  type <- in_list$brickr_object
  
  num_steps <- min(abs(round(num_steps)), 40)
  rows_per_step <- ceiling((max(image$ymax)-0.5) / (num_steps+1))
  
  if (type == "mosaic") {
    create_steps <- function(a, n_steps) {
      if(a < n_steps){
        image %>% 
          dplyr::group_by(brick_name) %>% 
          dplyr::filter(min(ymin) <= a*rows_per_step+(min(image$ymin)+0.5)) %>% 
          dplyr::ungroup() %>%
          dplyr::mutate(Step = paste("Step", stringr::str_pad(a, 2, pad = "0")))
      } else {
        image %>% 
          dplyr::mutate(Step = paste("Step", stringr::str_pad(a, 2, pad = "0")))
      }
    }
  }
  
  steps_for_table <- 1:num_steps %>% 
    purrr::map_df(create_steps, num_steps) %>% 
    dplyr::mutate(alpha = 1)
  
  all_x_max <- max(steps_for_table$xmax)
  all_x_min <- min(steps_for_table$xmin)
  all_y_max <- max(steps_for_table$ymax)
  all_y_min <- min(steps_for_table$ymin)
  
  plot_df <- dplyr::filter(steps_for_table, Step == step)
  
  res <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_rect(ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                                    fill = Lego_color, alpha = alpha), color = "#333333") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() + 
    ggplot2::coord_fixed(ratio = coord_ratio, expand = TRUE) +
    ggplot2::scale_y_continuous(limits = c(NA, all_y_max)) +
    #ggplot2::facet_wrap(~Step) +
    ggplot2::theme_minimal() +
    ggplot2::theme( panel.background = ggplot2::element_rect(fill = "#7EC0EE"),
                    strip.background = ggplot2::element_rect(fill = "#F7F18D"),
                    strip.text = ggplot2::element_text(color = "#333333", face = "bold"),
                    axis.line = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank(),
                    legend.position = "none")
  
  return(res)
}

build_pieces_steps <- function(pcs_step_df, step = "Step 01") {
  # if (step < 10) {
  #   step_chr <- paste0("Step 0", step)
  # } else {
  #   step_chr <- paste0("Step ", step)
  # }
  step_chr <- step
  
  pcs <- pcs_step_df %>%
    rename(n = n_step) %>%
    filter(Step == step_chr)
  
  pcs_coords <- dplyr::tibble(
    Brick_size = c("1 x 1", "2 x 1", "3 x 1", "4 x 1", "2 x 2", "4 x 2"),
    xmin = c(0, 0, 0, 0, 6, 6),
    xmax = c(1, 2, 3, 4, 8, 8),
    ymin = c(0, 2, 4, 6, 0, 3),
    ymax = c(1, 3, 5, 7, 2, 7)
  ) 
  
  #This function creates nodes in each brick for stud placement
  pcs_coords <- pcs_coords %>% 
    dplyr::mutate(studs = purrr::pmap(list(xmin, xmax, ymin, ymax), function(a, b, c, d){
      expand.grid(x=seq(a+0.5, b-0.5, by=1), 
                  y=seq(c+0.5, d-0.5, by=1))
    }))
  
  pcs2 <- pcs %>% 
    dplyr::arrange(Lego_color) %>% 
    dplyr::mutate(Lego_name = factor(Lego_name, 
                                     levels = c("Black", 
                                                unique(Lego_name)[!(unique(Lego_name) %in% c("Black", "White"))],
                                                "White"))) %>% 
    dplyr::left_join(pcs_coords, by = "Brick_size")
  
  coord_xlim <- c(-0.5, 10)
  facet_cols <- 5
  
  pcs2 %>% 
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin=xmin, xmax=xmax, ymin=-ymin, ymax=-ymax,
                                    fill = Lego_color), color = "#333333")+
    ggplot2::scale_fill_identity() +
    ggplot2::geom_point(data = pcs2 %>% tidyr::unnest(studs),
                        ggplot2::aes(x=x, y=-y), 
                        color = "#cccccc", alpha = 0.25, 
                        shape = 1, size = 2) +
    ggplot2::geom_text(
      ggplot2::aes(x = xmax + 0.25, y = -(ymin+ymax)/2, label = paste0("x", n)), 
      hjust = 0, vjust = 0.5, size = 3.5) +
    ggplot2::coord_fixed(xlim = coord_xlim) +
    ggplot2::labs(title = paste0("LEGO Bricks for ", step_chr)) +
    ggplot2::facet_wrap(~Lego_name, ncol=facet_cols) +
    ggplot2::theme_minimal() +
    ggplot2::theme( panel.background = ggplot2::element_rect(fill = "#7EC0EE"),
                    strip.background = ggplot2::element_rect(fill = "#F7F18D"),
                    strip.text = ggplot2::element_text(color = "#333333", face = "bold"),
                    axis.line = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank(),
                    panel.grid = ggplot2::element_blank())
  
  
  
}
