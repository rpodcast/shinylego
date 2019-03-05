devtools::load_all()

img_file <- "inst/images/kde_konqi_mascot.jpg"
image <- jpeg::readJPEG(img_file)
res <- scale_image(image, img_size = 48)

res <- scale_image(image, img_size = 48) %>%
  legoize() %>%
  collect_bricks(num_steps = 8)

display_set(res)

res <- generate_steps(res, num_steps = 10)

pcs_step <- count_bricks(res$steps, "Step 03")

# unique steps in the steps data frame
step_ids <- unique(res$steps$Step)

pcs_steps_all <- map_dfr(step_ids, ~count_bricks(df = res$steps, step_id = .x))

plot_instructions(res)
plot_instructions(res, step_id = "Step 02")

generate_instructions(res, num_steps = 10)

pieces <- table_pieces(res)

display_pieces(res, step_id = "Step 03")

res3d <- scale_image(image, img_size = 48) %>%
  legoize() %>%
  collect_bricks() %>%
  collect_3d()

display_3d(res3d)

library(magick)

orig <- image_read("Pictures/200px-Tux.svg.png")
orig_jpg <- image_convert(orig, format = "jpeg")

image_write(orig_jpg, "Pictures/tux_from_magick.jpeg")
