#' LEGO colors and their R, G, and B values
#' 
#' A dataset containing 39 LEGO brick colors and their associated red (R), green (G),
#' and blue (B) color values. These colors (and more) can be found in LEGO's official
#' [moulding colour palette](http://www.bartneck.de/wp-content/uploads/2016/09/2016-LEGO-color-palette.pdf) 
#' (2016 version). Note that this version of the data set excludes tranparent,
#' glow, and metallic bricks.
#' 
#' @format A data frame (tibble) with the following columns:
#' * `LEGONo`: Unique LEGO brick ID
#' * `Color`: Color name
#' * `R_lego`: Red color intensity proportion
#' * `G_lego`: Green color intensity propotion
#' * `B_lego`: Blue color intensity proportion
#' * `c_Palette2016`: Boolean for whether color is part of 2016 palette
#' * `c_Transparent`: Is the brick transparent?
#' * `c_Glow`: Is the brick considered a glow brick?
#' * `c_Metallic`: Is the brick considerend metallic?
#' * `t_BW`: Black or white color?
#' * `t_Classic`: Classic color?
#' * `t_Friends`: Part of friends collection?
#' * `w_weight`: ???
#' * `w_Classic`: ???
#' 
#' @examples
#' lego_colors
"lego_colors"

#' LEGO brick price data
#' 
#' A dataset with prices of plate bricks for the common sizes used in building the 
#' LEGO mosaics. These prices were derived from the [official LEGO shop store](https://shop.lego.com)
#' as of early 2019. 
#' 
#' @format A data frame (tibble) with the following columns:
#' * `type`: Type of brick
#' * `size`: Dimensions in format `a x b` measured in studs
#' * `shape`: Either square (`a x a`) or rectangle (`a x b`)
#' * `price`: Price for one unit of the specific brick (in U.S. dollars)
#' 
#' @examples
#' price_data
"price_data"
