devtools::load_all()

img_file <- "inst/images/kde_konqi_mascot.jpg"
image <- jpeg::readJPEG(img_file)
res <- scale_image(image, img_size = 48)

res <- scale_image(image, img_size = 48) %>%
  legoize() %>%
  collect_bricks()

display_set(res)

generate_instructions(res, num_steps = 10)

pieces <- table_pieces(res)

display_pieces(res)

res3d <- scale_image(image, img_size = 48) %>%
  legoize() %>%
  collect_bricks() %>%
  collect_3d()
