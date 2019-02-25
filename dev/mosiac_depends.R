# use itdepends package to analyze package dependencies for the mosiac code base
# https://speakerdeck.com/jimhester/it-depends

library(tidyverse)

# install.packages("remotes")
# remotes::install_github("jimhester/itdepends")
library(itdepends)

# LEGO Mosiac project repo: https://github.com/ryantimpe/LEGOMosaics
# Note: Must move all R code to a new subdirectory called "R" in the LEGOMosiacs
# project root in order for itdepends to function correctly

# change local path to match your setup
res <- itdepends::dep_usage_proj(path = "/home/eric/rpodcast_code/LEGOMosaics")

res %>%
  group_by(pkg) %>%
  count(fun) %>%
  top_n(1) %>%
  arrange(desc(n)) %>%
  View(.)
