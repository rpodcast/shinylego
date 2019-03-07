devtools::load_all()

img_file <- "inst/images/kde_konqi_mascot.jpg"
image <- jpeg::readJPEG(img_file)

res <- scale_image(image, img_size = 48) %>%
  legoize() %>%
  collect_bricks(num_steps = 8)


# analyze metrics
pieces <- res$pieces %>%
  select(Brick_size) %>%
  distinct() %>%
  pull() %>%
  length()

pieces_cost <- res$pieces %>%
  group_by(Brick_size) %>%
  summarize(n_total = sum(n)) %>%
  ungroup %>%
  left_join(
    price_data,
    by = c("Brick_size" = "size")
  ) %>%
  mutate(brick_type_cost = n_total * price)

data("price_data")
  

display_set(res)

res_steps <- generate_steps(res$Img_bricks, num_steps = 10)

pcs_step <- count_bricks(res_steps, "Step 03")

# unique steps in the steps data frame
step_ids <- unique(res_steps$Step)

pcs_steps_all <- step_pieces(res_steps)
pcs_step <- step_pieces(res_steps, "Step 02")

table_pieces(res$pieces)
mytable <- table_pieces(pcs_steps_all, "Step 02")

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
