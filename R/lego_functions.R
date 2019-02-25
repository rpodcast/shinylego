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


#2 Legoize - Convert image Lego colors -----
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
convert_to_lego_colors <- function(R, G, B) {
  data(lego_colors)
  
  lego_colors %>% 
    mutate(dist = ((R_lego - R)^2 + (G_lego - G)^2 + (B_lego - B)^2)^(1/2)) %>% 
    top_n(-1, dist) %>% 
    mutate(Lego_color = grDevices::rgb(R_lego, G_lego, B_lego)) %>% 
    select(Lego_name = Color, Lego_color)
}

#' Title
#'
#' @param image_list 
#' @param theme 
#' @param contrast 
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @return
#' @export
#'
#' @examples
legoize <- function(image_list, theme = "default", contrast = 1){
  in_list <- image_list
  
  data(lego_colors)
  color_table <- lego_colors
  
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

#3 collect_bricks - Combine bricks into larger ones ----
#' Title
#'
#' @param image_list 
#' @param mosaic_type 
#'
#' @import dplyr
#' @import tidyr
#' @return
#' @export
#'
#' @examples
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

#3a display_set  - plot output of collect_bricks() ----
#' Title
#'
#' @param image_list 
#' @param title 
#'
#' @import ggplot2
#' @return
#' @export
#'
#' @examples
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
    theme_lego
  
  return(img)
} 

#4 Instructions ----
#' Title
#'
#' @param image_list 
#' @param num_steps 
#'
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @return
#' @export
#'
#' @examples
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
    theme_lego
}

#5 Piece count ----
#Print as data frame
#' Title
#'
#' @param image_list 
#'
#' @import dplyr
#' @import tidyr
#' @return
#' @export
#'
#' @examples
table_pieces <- function(image_list){
  pcs <- image_list$pieces
  
  pcs %>% 
    select(-Lego_color) %>% 
    spread(Brick_size, n, fill = 0) %>% 
    rename(`LEGO Brick Color` = Lego_name)
}

#Print as image
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
    theme_lego +
    theme(
      panel.grid = element_blank()
    )
}
