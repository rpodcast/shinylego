theme_lego <- function() {
  theme_lego <- theme(panel.background = element_rect(fill = "#7EC0EE"),
                      strip.background = element_rect(fill = "#F7F18D"),
                      strip.text = element_text(color = "#333333", face = "bold"),
                      axis.line = element_blank(),
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.y = element_blank())
  
  return(theme_lego)
}

#' Create a scaled version of jpeg image
#'
#' @param image raster array of jpeg image file as produced by `jpeg::readJPEG`.
#' @param img_size dimensions of the mosaic base plate. For a square base, supply
#'   a single integer. For a rectangle base, supply a vector with two integers in 
#'   the form of `c(width, height`).
#' @param brightness Brightness adjustment threshold. For a darker mosaic, enter 
#'   a number between 0 and 1. For a brighter mosaic, enter a number greater than 1.
#'   Default is `1`, meaning no adjustment.
#' @param warhol ??? Default is `1:3`
#'
#' @import dplyr
#' @import tidyr
#' @return list with object called `Img_scaled` representing the scaled image. 
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48)
scale_image <- function(image, img_size, brightness = 1, warhol = 1:3){
  #Adjust brightness
  if(brightness < 0 ){stop("brightness should be a positive value. Use 1 for no change, >1 for lighter, <1 for darker.")}
  image_b <- image*brightness
  image_b[image_b>1] <- 1
  
  col_chan <- order(warhol[1:3])
  
  #Convert image to a data frame with RGB values
  img <- bind_rows(
    list(
      (as.data.frame(image_b[, , col_chan[1]]) %>% 
         mutate(y=row_number(), channel = "R")),
      (as.data.frame(image_b[, , col_chan[2]]) %>% 
         mutate(y=row_number(), channel = "G")),
      (as.data.frame(image_b[, , col_chan[3]]) %>% 
         mutate(y=row_number(), channel = "B"))
    )
  ) %>% 
    gather(x, value, -y, -channel) %>% 
    mutate(x = as.numeric(gsub("V", "", x))) %>% 
    spread(channel, value)
  
  img_size <- round(img_size, 0)
  
  #Wide or tall image? Shortest side should be `img_size` pixels
  if(max(img$x) > max(img$y)){
    img_scale_x <-  max(img$x) / max(img$y)
    img_scale_y <- 1
  } else {
    img_scale_x <- 1
    img_scale_y <-  max(img$y) / max(img$x)
  }
  
  #If only 1 img_size value, create a square image
  if(length(img_size) == 1){
    img_size2 <- c(img_size, img_size)
  } else {
    img_size2 <- img_size[1:2]
    img_scale_x <- 1
    img_scale_y <- 1
  }
  
  #Rescale the image
  img2 <- img %>% 
    mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_size2[2]*img_scale_y + 1,
           x_scaled = (x - min(x))/(max(x)-min(x))*img_size2[1]*img_scale_x + 1) %>% 
    select(-x, -y) %>% 
    group_by(y = ceiling(y_scaled), x = ceiling(x_scaled)) %>% 
    #Get average R, G, B and convert it to hexcolor
    summarize_at(vars(R, G, B), funs(mean(.))) %>% 
    rowwise() %>% 
    mutate(color = rgb(R, G, B)) %>% 
    ungroup() %>% 
    #Center the image
    filter(x <= median(x) + img_size2[1]/2, x > median(x) - img_size2[1]/2,
           y <= median(y) + img_size2[2]/2, y > median(y) - img_size2[2]/2) %>%
    #Flip y
    mutate(y = (max(y) - y) + 1)
  
  out_list <- list()
  out_list[["Img_scaled"]] <- img2
  
  return(out_list)
}

#' Convert raw R,G,B values to LEGO version
#' 
#' Replaces a color defined by RGB values to a simular color in the 
#' `lego_colors` data set using the shortest Euclidean distance.  
#'
#' @param R Red color intensity value
#' @param G Green color intensity value
#' @param B Blue color intensity value
#'
#' @return data frame 
#' @importFrom grDevices rgb
#' @import dplyr
#' @export
#'
#' @examples
#' data(lego_colors, package = "shinylego")
#' new_colors <- convert_to_lego_colors(R = 30, G = 74, B = 38, lego_colors)
convert_to_lego_colors <- function(R, G, B, lego_colors) {
  
  lego_colors %>% 
    mutate(dist = ((R_lego - R)^2 + (G_lego - G)^2 + (B_lego - B)^2)^(1/2)) %>% 
    top_n(-1, dist) %>% 
    mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
    select(Lego_name = Color, Lego_color)
}

#' Create LEGO-ized version of image
#'
#' @param image_list list object containing the `Img_scaled` data frame
#'   as produced by the [scale_image()] function.
#' @param theme string indicating whether to use the `default` color
#'   theme or `bw` (grayscale) theme.
#' @param contrast Adjustment for color allocation when using grayscale theme.
#'   Must be a positive number.  Default is 1, meaning no adjustment.
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @return list with modified `Img_lego` object (the original image lego-ized)
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48) %>%
#'   legoize()
legoize <- function(image_list, theme = "default", contrast = 1){
  in_list <- image_list
  
  if(theme == "default"){
    #Speed up calc by round pixel to nearest 1/20 & only calculating unique
    mosaic_colors <- in_list$Img_scaled %>% 
      mutate_at(vars(R, G, B), list(~round(.*20)/20)) %>% 
      select(R, G, B) %>% 
      distinct() %>% 
      mutate(lego = purrr::pmap(list(R, G, B), convert_to_lego_colors, lego_colors)) %>% 
      unnest(lego)
    
    img <- in_list$Img_scaled %>% 
      mutate_at(vars(R, G, B), list(~round(.*20)/20)) %>%
      left_join(mosaic_colors, by = c("R", "G", "B"))
    
  } else if (theme == "bw"){
    #Black and white is simpler... cut the colors into 4 groups, then assign lightest = white, darkest = black
    bw_colors <- lego_colors %>% 
      filter(t_BW) %>% 
      arrange((R_lego + G_lego + B_lego)) %>% 
      mutate(Lego_color = rgb(R_lego, G_lego, B_lego))
    
    img <- in_list$Img_scaled %>% 
      mutate(shade = (R+G+B)/3,
             shade = shade ^ contrast) %>% 
      mutate(shade_bw = as.numeric(as.factor(cut(shade, 4)))) %>% 
      mutate(Lego_name = bw_colors$Color[shade_bw],
             Lego_color = bw_colors$Lego_color[shade_bw]) %>% 
      select(-starts_with("shade"))
    
  }
  in_list[["Img_lego"]] <- img
  
  return(in_list)
  
}

#' Collect required bricks for mosaic
#'
#' @param image_list list object containing the `Img_scaled` data frame
#'   as produced by the [legoize()] function.
#' @param mosaic_type String for mosaic type to produce. A `flat` mosaic
#'   places a single layer of LEGO plates on a base plate with study-side up.
#'   A `stacked` mosaic staggers bricks and places them horizontally. 
#' 
#' @import dplyr
#' @import tidyr
#' @return list with following components:
#'   * `Img_bricks`: Updated LEGO image
#'   * `ID_bricks`: Data frame with brick IDs
#'   * `mosaic_type`: Type of mosaic specfied by user
#'   * `pieces`: Data frame with number of bricks needed`
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48) %>%
#'   legoize() %>%
#'   collect_bricks()
collect_bricks <- function(image_list, mosaic_type = "flat"){
  in_list <- image_list
  
  if(mosaic_type == "flat"){
    img <- in_list$Img_lego %>% 
      select(x, y, Lego_name, Lego_color) %>% 
      #4x2 bricks - horizontal
      group_by(xg = x %/% 4, yg = y %/% 2) %>% 
      mutate(g_1_x4y2_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 8,
                                 paste0("x4y2_", "x", min(x), "_y", min(y)), NA)) %>% 
      #4x2 bricks - vertical
      ungroup() %>% group_by(xg = x %/% 2, yg = y %/% 4) %>% 
      mutate(g_2_x2y4_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 8,
                                 paste0("x2y4_", "x", min(x), "_y", min(y)), NA)) %>% 
      #2x2 bricks
      ungroup() %>% group_by(xg = x %/% 2, yg = y %/% 2) %>% 
      mutate(g_5_x2y2_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                                 paste0("x2y2_", "x", min(x), "_y", min(y)), NA)) %>% 
      #4x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 4, yg = y ) %>% 
      mutate(g_7_x4y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                                 paste0("x4y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #4x1 bricks -  vertical
      ungroup() %>% group_by(xg = x, yg = y %/% 4) %>% 
      mutate(g_8_x1y4_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                                 paste0("x1y4_", "x", min(x), "_y", min(y)), NA)) %>% 
      #3x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 3, yg = y ) %>% 
      mutate(g_7_x3y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 3,
                                 paste0("x3y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #3x1 bricks -  vertical
      ungroup() %>% group_by(xg = x, yg = y %/% 3) %>% 
      mutate(g_8_x1y3_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 3,
                                 paste0("x1y3_", "x", min(x), "_y", min(y)), NA)) %>% 
      #2x1 bricks - horizontal
      ungroup() %>% group_by(xg = x %/% 2, yg = y ) %>% 
      mutate(g_9_x2y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 2,
                                 paste0("x2y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #2x1 bricks -  vertical
      ungroup() %>% group_by(xg = x, yg = y %/% 2) %>% 
      mutate(g_10_x1y2_1 = ifelse(length(unique(Lego_name)) == 1 & n() == 2,
                                  paste0("x1y2_", "x", min(x), "_y", min(y)), NA)) %>% 
      ungroup() %>% 
      #1x1
      mutate(g_11_x1y1_0 = paste0("x1y1_", "x", x, "_y", y)) %>% 
      select(-xg, -yg)
  }
  else if(mosaic_type == "stacked"){
    img <- in_list$Img_lego %>% 
      select(x, y, Lego_name, Lego_color) %>% 
      #4x1 bricks - horizontal
      ungroup() %>% group_by(xg = (x + y %% 4) %/% 4, yg = y ) %>% 
      mutate(g_7_x4y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 4,
                                 paste0("x4y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #3x1 bricks - horizontal
      ungroup() %>% group_by(xg = (x + y %% 3) %/% 3, yg = y ) %>% 
      mutate(g_7_x3y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 3,
                                 paste0("x3y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      #2x1 bricks - horizontal
      ungroup() %>% group_by(xg = (x + y %% 2) %/% 2, yg = y ) %>% 
      mutate(g_9_x2y1_0 = ifelse(length(unique(Lego_name)) == 1 & n() == 2,
                                 paste0("x2y1_", "x", min(x), "_y", min(y)), NA)) %>% 
      ungroup() %>% 
      #1x1
      mutate(g_11_x1y1_0 = paste0("x1y1_", "x", x, "_y", y)) %>% 
      select(-xg, -yg)
  }
  else(stop("Use mosaic_type = 'flat' or 'stacked'"))
  
  img2 <- img %>% 
    gather(Brick, brick_id, dplyr::starts_with("g_")) %>% 
    #Only keep first Brick group has a name
    group_by(x, y) %>% 
    filter(Brick == Brick[min(which(!is.na(brick_id)))]) %>% 
    ungroup() %>% 
    # min/max coord for geom_rect()
    group_by(Brick, brick_id, Lego_color, Lego_name) %>% 
    summarise(xmin = min(x)-0.5, xmax = max(x)+0.5,
              ymin = min(y)-0.5, ymax = max(y)+0.5) %>% 
    ungroup()
  
  brick_ids <- img %>% 
    gather(Brick, brick_id, dplyr::starts_with("g_")) %>% 
    #Only keep first Brick group has a name
    group_by(x, y) %>% 
    filter(Brick == Brick[min(which(!is.na(brick_id)))]) %>% 
    ungroup() 
  
  # This is very brute-force. Probably a much cleaner way to do this
  pcs <- img2 %>% 
    select(Brick, brick_id, Lego_name, Lego_color) %>% 
    distinct() %>% 
    separate(Brick, c("g", "gn", "size", "gi")) %>% 
    select(-dplyr::starts_with("g")) %>% 
    mutate(size1 = as.numeric(substr(size, 2, 2)), 
           size2 = as.numeric(substr(size, 4, 4))) %>% 
    mutate(Brick_size = ifelse(size1>size2, paste(size1, "x", size2), paste(size2, "x" , size1))) %>% 
    count(Brick_size, Lego_name, Lego_color) 
  
  #Replace "x 1" bricks with "x 2". More likely to be used for a stacked mosaic
  if(mosaic_type == "stacked"){
    pcs <- pcs %>% 
      mutate(Brick_size = gsub("x 1", "x 2", Brick_size, fixed = TRUE))
  }
  
  in_list[["Img_bricks"]] <- img2
  in_list[["ID_bricks"]] <- brick_ids
  in_list[["mosaic_type"]] <- mosaic_type
  in_list[["pieces"]] <- pcs
  
  return(in_list)
}

#' Render the LEGO mosaic as a `ggplot`
#'
#' @param image_list List of objects produced by [collect_bricks()]
#' @param title Optional title for plot
#'
#' @import ggplot2
#' @return Plot object
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48) %>%
#'   legoize() %>%
#'   collect_bricks()
#'   
#' display_set(res)
display_set <- function(image_list, title=NULL){
  in_list <- image_list
  image <- in_list$Img_bricks
  type <- in_list$mosaic_type
  
  coord_x <- c(min(image$xmin)+0.5, max(image$xmax)-0.5)
  coord_y <- c(min(image$ymin)+0.5, max(image$ymax)-0.5)
  
  img <- ggplot(image) +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                  fill = Lego_color), color = "#333333")+
    scale_fill_identity()
  
  if(type == "flat"){
    img <- img + geom_point(data = expand.grid(x=coord_x[1]:coord_x[2], y=coord_y[1]:coord_y[2]),
                            aes(x=x, y=y), color = "#333333", alpha = 0.2, shape = 1, size = 2)   +
      coord_fixed(expand = FALSE) 
  } else {
    img <- img +
      coord_fixed(ratio = 6/5, expand = FALSE)
  }
  
  img <- img+
    labs(title = title) +
    theme_minimal() +
    theme_lego()
  
  return(img)
} 

#' Produce tidy data set for building instructions
#'
#' @param image_list List of objects produced by [collect_bricks()]
#' @param num_steps Number of steps for the instructions. Default is 6.
#'
#' @import dplyr
#' @import purrr
#' @return list with new slot containing `tibble` of the instructions
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48) %>%
#'   legoize() %>%
#'   collect_bricks()
#'   
#' res_steps <- generate_steps(res)
#' 
generate_steps <- function(image_list, num_steps=6) {
  in_list <- image_list
  image <- in_list$Img_bricks
  type <- in_list$mosaic_type
  
  num_steps <- min(round(num_steps), 40)
  
  rows_per_step <- ceiling((max(image$ymax)-0.5) / (num_steps+1))
  
  create_steps <- function(a, n_steps) {
    if(a < n_steps){
      image %>% 
        group_by(brick_id) %>% 
        filter(min(ymin) <= a*rows_per_step+(min(image$ymin)+0.5)) %>% 
        ungroup() %>%
        mutate(Step = paste("Step", (if(a<10){paste0('0', a)}else{a})))
    } else {
      image %>% 
        mutate(Step = paste("Step", (if(a<10){paste0('0', a)}else{a})))
    }
    
  }
  
  #Ratio of the "pixels" is different for flat or stacked bricks
  if(type == "flat"){
    coord_ratio <- 1
  } else {
    coord_ratio <- 6/5
  }
  
  res <- 1:num_steps %>% 
    map(create_steps, num_steps) %>% 
    bind_rows()
  
  in_list[['steps']] <- res
  return(in_list)
}

#' Display plot with instructions at all or specific step
#'
#' @param steps_df `tibble` of the bricks required at each step
#' @param step_id Optional index of step to print. If `NULL` 
#'   (default), all steps are plotted.
#'
#' @return
#' @export
#'
#' @examples
plot_instructions <- function(image_list, step_id = NULL) {
  
  in_list <- image_list
  type <- in_list$mosaic_type
  
  all_x_max <- max(in_list$steps$xmax)
  all_x_min <- min(in_list$steps$xmin)
  all_y_max <- max(in_list$steps$ymax)
  all_y_min <- min(in_list$steps$ymin)

  if (!is.null(step_id)) {
    steps_df <- filter(in_list[['steps']], Step %in% step_id)
  } else {
    steps_df <- in_list[['steps']]
  }
  
  #Ratio of the "pixels" is different for flat or stacked bricks
  if(type == "flat"){
    coord_ratio <- 1
  } else {
    coord_ratio <- 6/5
  }
  
  p <- ggplot(steps_df) +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                  fill = Lego_color), color = "#333333")+
    scale_fill_identity() +
    coord_fixed(ratio = coord_ratio, expand = FALSE)
  
  if (is.null(step_id) || length(step_id) > 1) {
    p <- p +
      facet_wrap(~Step)
  } else {
    p <- p +
      scale_y_continuous(limits = c(NA, all_y_max))
  }
  
  # apply theme
  p <- p +
    theme_minimal()+
    theme_lego()
  
  return(p)
}

#' Produce instructions for building LEGO mosaic
#'
#' @param image_list List of objects produced by [collect_bricks()]
#' @param num_steps Number of steps for the instructions. Default is 6.
#'
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @return `ggplot2` plot object with each facet being a particular step
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48) %>%
#'   legoize() %>%
#'   collect_bricks()
#'   
#' generate_instructions(res)
generate_instructions <- function(image_list, num_steps=6) {
  in_list <- image_list
  image <- in_list$Img_bricks
  type <- in_list$mosaic_type
  
  num_steps <- min(round(num_steps), 40)
  
  rows_per_step <- ceiling((max(image$ymax)-0.5) / (num_steps+1))
  
  create_steps <- function(a, n_steps) {
    if(a < n_steps){
      image %>% 
        group_by(brick_id) %>% 
        filter(min(ymin) <= a*rows_per_step+(min(image$ymin)+0.5)) %>% 
        ungroup() %>%
        mutate(Step = paste("Step", (if(a<10){paste0('0', a)}else{a})))
    } else {
      image %>% 
        mutate(Step = paste("Step", (if(a<10){paste0('0', a)}else{a})))
    }
    
  }
  
  #Ratio of the "pixels" is different for flat or stacked bricks
  if(type == "flat"){
    coord_ratio <- 1
  } else {
    coord_ratio <- 6/5
  }
  
  1:num_steps %>% 
    map(create_steps, num_steps) %>% 
    bind_rows() %>% 
    ggplot() +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                  fill = Lego_color), color = "#333333")+
    scale_fill_identity() +
    coord_fixed(ratio = coord_ratio, expand = FALSE) +
    facet_wrap(~Step) +
    theme_minimal()+
    theme_lego()
}

#' Create printer-friendly version of required bricks
#'
#' @param image_list List of objects produced by [collect_bricks()]
#'
#' @import dplyr
#' @import tidyr
#' @return data frame (tibble) with columns for each brick dimension and rows
#'   for each unique brick color in mosaic.
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48) %>%
#'   legoize() %>%
#'   collect_bricks()
#'   
#' table_pieces(res)
table_pieces <- function(image_list){
  pcs <- image_list$pieces
  
  pcs %>% 
    select(-Lego_color) %>% 
    spread(Brick_size, n, fill = 0) %>% 
    rename(`LEGO Brick Color` = Lego_name)
}

#' Visualize required pieces for mosaic
#'
#' @param image_list List of objects produced by [collect_bricks()]
#'
#' @import ggplot2
#' @import dplyr
#' @import tibble
#' @import purrr
#' @return `ggplot` plot object showing number of required bricks faceted by 
#'   brick color.  Each facet contains all of the unique brick types required
#'   for that color.
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48) %>%
#'   legoize() %>%
#'   collect_bricks()
#'   
#' display_pieces(res)
display_pieces <- function(image_list){
  in_list <- image_list
  pcs <- in_list$pieces
  
  if(in_list$mosaic_type == "flat"){
    pcs_coords <- tibble(
      Brick_size = c("1 x 1", "2 x 1", "3 x 1", "4 x 1", "2 x 2", "4 x 2"),
      xmin = c(0, 0, 0, 0, 6, 6),
      xmax = c(1, 2, 3, 4, 8, 8),
      ymin = c(0, 2, 4, 6, 0, 3),
      ymax = c(1, 3, 5, 7, 2, 7)
    ) 
  } else {
    pcs_coords <- tibble(
      Brick_size = c("1 x 2", "2 x 2", "3 x 2", "4 x 2"),
      xmin = c(0, 5, 5, 0),
      xmax = c(2, 7, 7, 2),
      ymin = c(0, 0, 3, 2),
      ymax = c(1, 2, 6, 6)
    ) 
  }
  #This function creates nodes in each brick for stud placement
  pcs_coords <- pcs_coords %>% 
    mutate(studs = purrr::pmap(list(xmin, xmax, ymin, ymax), function(a, b, c, d){
      expand.grid(x=seq(a+0.5, b-0.5, by=1), 
                  y=seq(c+0.5, d-0.5, by=1))
    }))
  
  pcs2 <- pcs %>% 
    arrange(Lego_color) %>% 
    mutate(Lego_name = factor(Lego_name, 
                              levels = c("Black", 
                                         unique(Lego_name)[!(unique(Lego_name) %in% c("Black", "White"))],
                                         "White"))) %>% 
    left_join(pcs_coords, by = "Brick_size")
  
  if(in_list$mosaic_type == "flat"){
    coord_xlim <- c(-0.5, 10)
    facet_cols <- 5
  } else {
    coord_xlim <- c(-0.5, 9)
    facet_cols <- 6
  }
  
  pcs2 %>% 
    ggplot() +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=-ymin, ymax=-ymax,
                  fill = Lego_color), color = "#333333")+
    scale_fill_identity() +
    geom_point(data = pcs2 %>% unnest(studs),
               aes(x=x, y=-y), color = "#cccccc", alpha = 0.25, 
               shape = 1, size = 2) +
    geom_text(aes(x = xmax + 0.25, y = -(ymin+ymax)/2, label = paste0("x", n)), 
              hjust = 0, vjust = 0.5, size = 3.5) +
    coord_fixed(xlim = coord_xlim) +
    labs(title = (if(in_list$mosaic_type == "stacked"){
      "Suggested LEGO Bricks"
    }else{"Suggested LEGO Plates"}),
    caption = (if(in_list$mosaic_type == "stacked"){
      "Mosaic is 2-bricks deep. Can substitute 2-stud bricks for 1-stud alternatives for a thinner mosaic."}else{""})) +
    facet_wrap(~Lego_name, ncol=facet_cols) +
    theme_minimal()+
    theme_lego() +
    theme(
      panel.grid = element_blank()
    )
}

#' Translate 2D mosaic information to 3D attributes
#'
#' @param image_list List of objects produced by [collect_bricks()]. Note that
#'   only flat mosaics are supported.
#' @param mosaic_height Height of bricks in image. Default is 6.
#' @param highest_el Color of highest elevation bricks. Supported values are 
#'   `light` (default) and `dark`.
#'
#' @import dplyr
#' @import tidyr
#' 
#' @return list with `image_list` contents augmented with following components:
#'   * `threed_elevation`
#'   * `threed_hillshade` 
#' @export
#'
#' @examples
#' img_file <- system.file("images", "kde_konqi_mascot.jpg", package = "shinylego")
#' image <- jpeg::readJPEG(img_file)
#' res <- scale_image(image, img_size = 48) %>%
#'   legoize() %>%
#'   collect_bricks() %>%
#'   collect_3d()
#' 
collect_3d <- function(image_list, mosaic_height = 6, highest_el = "light"){
  #Get previous data
  in_list <- image_list
  
  if(in_list$mosaic_type != "flat")stop("3D mosaics can only be generated with 'flat' mosaics. Set this input in the 'collect_bricks' function.")
  
  BrickIDs <- in_list$ID_bricks
  img_lego <- in_list$Img_lego
  
  #Number of 'pixels' on a side of a single-stud brick. I think this should be fixed for now
  ex_size <- 15
  
  lego_expand <- img_lego %>%
    select(x, y, Lego_name, Lego_color) %>% 
    mutate(stud_id = row_number()) 
  
  lego_expand2 <- expand.grid(x = (min(lego_expand$x)*ex_size):(max(lego_expand$x+1)*ex_size),
                              y = (min(lego_expand$y)*ex_size):(max(lego_expand$y+1)*ex_size)) %>% 
    mutate(x_comp = x %/% ex_size,
           y_comp = y %/% ex_size) %>% 
    left_join(lego_expand %>% rename(x_comp = x, y_comp = y), by = c("x_comp", "y_comp")) %>% 
    left_join(BrickIDs %>% select(brick_id, x_comp = x, y_comp = y), by = c("x_comp", "y_comp")) %>% 
    select(-x_comp, -y_comp) %>% 
    left_join(lego_colors %>% select(Lego_name = Color, R_lego, G_lego, B_lego), by = "Lego_name") %>% 
    do(
      if(highest_el == "dark"){
        mutate(., elevation = (1-((R_lego + G_lego + B_lego )/3)) * 1000)
      } else {
        mutate(., elevation = (((R_lego + G_lego + B_lego )/3)) * 1000)
      }
    ) %>% 
    #Round elevation to nearest 1/height
    mutate(elevation = as.numeric(as.factor(cut(elevation, mosaic_height)))) %>% 
    mutate(y = max(y)-y) %>% 
    filter(!is.na(elevation)) %>% 
    #Calculate stud placement... radius of 1/3 and height of 0.5 plate
    group_by(stud_id) %>% 
    mutate(x_mid = median(x), y_mid = median(y),
           stud = ((x-x_mid)^2 + (y-y_mid)^2)^(1/2) < ex_size/3) %>% 
    ungroup() %>% 
    mutate(elevation = ifelse(stud, elevation+0.5, elevation)) %>% 
    mutate_at(vars(R_lego, G_lego, B_lego), list(~ ifelse(stud, .-0.1, .))) %>% 
    mutate_at(vars(R_lego, G_lego, B_lego), list(~ ifelse(. < 0, 0, .)))
  
  #Elevation Matrix
  lego_elmat <- lego_expand2 %>% 
    select(x, y, elevation) %>% 
    spread(y, elevation) %>% 
    select(-x) %>% 
    as.matrix()
  
  #Hillshade matrix
  lego_hillshade_m <- array(dim = c(length(unique(lego_expand2$y)), length(unique(lego_expand2$x)), 3))
  
  lego_expand_color <- lego_expand2 %>% 
    group_by(brick_id) %>% 
    #This darkens the edge of each brick, to look like they are separated
    mutate_at(vars(R_lego, G_lego, B_lego), 
              list(~ ifelse((x == min(x) | y == min(y) | x == max(x) | y == max(y)), .*0.4, .))) %>% 
    ungroup()
  
  lego_hillshade_m[,,1] <- lego_expand_color %>% 
    select(x, y, R_lego) %>% 
    spread(x, R_lego) %>% 
    select(-y) %>% 
    as.matrix()
  
  lego_hillshade_m[,,2] <- lego_expand_color %>% 
    select(x, y, G_lego) %>% 
    spread(x, G_lego) %>% 
    select(-y) %>% 
    as.matrix()
  
  lego_hillshade_m[,,3] <- lego_expand_color %>% 
    select(x, y, B_lego) %>% 
    spread(x, B_lego) %>% 
    select(-y) %>% 
    as.matrix()
  
  #Return
  in_list[["threed_elevation"]] <- lego_elmat
  in_list[["threed_hillshade"]] <- lego_hillshade_m
  
  return(in_list)
  
}

#' Display 3D render of mosaic
#'
#' @param image_list List of objects produced by [collect_3d()]
#' @param ... Additional parameters passed to [rayshader::plot_3d()]
#'
#' @export
display_3d <- function(image_list, ...){
  image_list$`threed_hillshade`%>%
    rayshader::plot_3d(image_list$`threed_elevation`, zscale=0.125, ...)
}

