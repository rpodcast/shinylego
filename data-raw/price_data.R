# processing for price_data.csv file

# load packages ----
library(readr)
library(dplyr)

# perform import ----
price_data <- readr::read_csv("data-raw/price_data.csv", 
                               col_types = cols(
                                 type = col_character(),
                                 size = col_character(),
                                 shape = col_character(),
                                 price = col_double()
                               ))

usethis::use_data(price_data, overwrite = TRUE)
