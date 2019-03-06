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

