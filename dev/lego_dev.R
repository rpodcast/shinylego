devtools::load_all()

img_file <- "inst/images/kde_konqi_mascot.jpg"
image <- jpeg::readJPEG(img_file)

res <- scale_image(image, img_size = 48) %>%
  legoize() %>%
  collect_bricks(num_steps = 8)

display_set(res)

res_steps <- generate_steps(res$Img_bricks, num_steps = 10)

pcs_step <- count_bricks(res_steps, "Step 03")




# unique steps in the steps data frame
step_ids <- unique(res_steps$Step)

pcs_steps_all <- step_pieces(res_steps)
pcs_step <- step_pieces(res_steps, "Step 02")

plot_instructions(res_steps)
plot_instructions(res_steps, step_id = "Step 01")

generate_instructions(res, num_steps = 10)

pieces <- table_pieces(res)

display_pieces(pcs_steps_all, step_id = "Step 03")
display_pieces(res)
res3d <- scale_image(image, img_size = 48) %>%
  legoize() %>%
  collect_bricks() %>%
  collect_3d()

display_3d(res3d)

library(magick)

orig <- image_read("Pictures/200px-Tux.svg.png")
orig_jpg <- image_convert(orig, format = "jpeg")

image_write(orig_jpg, "Pictures/tux_from_magick.jpeg")
