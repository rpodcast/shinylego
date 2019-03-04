devtools::load_all()
library(magick)

# use magick package to resize animated gif
# DID NOT WORK
img_file <- "inst/app/www/source.gif"
img_obj <- image_read(img_file) %>%
  image_scale("400") %>%
  image_write("inst/app/www/lego_load.gif", format = "gif")
