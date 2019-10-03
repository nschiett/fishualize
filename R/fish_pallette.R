#' Original fish color database
#'
#' A dataset containing some colour palettes inspired by fish species
#'
#'
#'@format A data frame containing all the colours used in the palette:
#'\itemize{
#'   \item option: It is intended to be a general option for choosing the specific colour palette.
#'   \item hex: hex color code
#'}
"fishcolors"



#' Available Palettes.
#'
#' This function returns a vector containing the names of all the available palettes in the 'fishualize' package.
#'
#' @return \code{fish_palettes} returns a character vector with the names of the fish palettes available to use.
#'
#'
#'
#' @examples
#' fish_palettes()
#'
#' @rdname fish_palettes
#' @export

fish_palettes <- function(){
  return(sort(unique(fishcolors$option)))
}

#'
#'
#'
#'
#' fish Colour Map.
#'
#' This function creates a vector of \code{n} equally spaced colors along the
#' 'fish colour map' of your selection
#'
#' @param n The number of colors (\eqn{\ge 1}) to be in the palette.
#'
#' @param alpha	The alpha transparency, a number in [0,1], see argument alpha in
#' \code{\link[grDevices]{hsv}}.
#'
#' @param begin The (corrected) hue in [0,1] at which the fish colormap begins.
#'
#' @param end The (corrected) hue in [0,1] at which the fish colormap ends.
#'
#' @param direction Sets the order of colors in the scale. If 1, the default, colors
#' are ordered from darkest to lightest. If -1, the order of colors is reversed.
#'
#' @param option A character string indicating the colourmap from a option to use.
#'
#'
#' @return \code{fish} returns a character vector, \code{cv}, of color hex
#' codes. This can be used either to create a user-defined color palette for
#' subsequent graphics by \code{palette(cv)}, a \code{col =} specification in
#' graphics functions or in \code{par}.
#'
#'
#'
#' Semi-transparent colors (\eqn{0 < alpha < 1}) are supported only on some
#' devices: see \code{\link[grDevices]{rgb}}.
#'
#' @examples
#' library(ggplot2)
#' library(hexbin)
#'
#' dat <- data.frame(x = rnorm(1e4), y = rnorm(1e4))
#' ggplot(dat, aes(x = x, y = y)) +
#'   geom_hex() +
#'   coord_fixed() +
#'   scale_fill_gradientn(colours = fish(128, option = 'Ostracion_cubicus'))
#'
#' pal <- fish(256, option = "Thalassoma_hardwicke", direction = -1)
#' image(volcano, col = pal)
#'
#' @rdname fish
#' @export
#'
fish <- function(n, alpha = 1, begin = 0, end = 1, direction = 1, option = "Chlorurus_microrhinos") {


  if (begin < 0 | begin > 1 | end < 0 | end > 1) {
    stop("begin and end must be in [0,1]")
  }

  if (abs(direction) != 1) {
    stop("direction must be 1 or -1")
  }

  if (direction == -1) {
    tmp <- begin
    begin <- end
    end <- tmp
  }

  map <- fishcolors[fishcolors$option == option, ]

  map_cols <- map$hex
  fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
  cols <- fn_cols(seq(begin, end, length.out = n)) / 255
  grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha)
}



#' @rdname fish
#'
#' @export
fish_pal <- function(alpha = 1, begin = 0, end = 1, direction = 1, option = 'Ostracion_cubicus') {


  function(n) {
    fish(n, alpha, begin, end, direction, option)
  }
}


#' @rdname scale_fish
#'
#' @importFrom ggplot2 scale_fill_gradientn scale_color_gradientn discrete_scale
#'
#' @export
scale_color_fish <- function(option = 'Ostracion_cubicus', ..., alpha = 1, begin = 0, end = 1, direction = 1,
                           discrete = FALSE) {

  if (discrete) {
    discrete_scale("colour", "fish", fish_pal(alpha, begin, end, direction, option), ...)
  } else {
    scale_color_gradientn(colours = fish(256, alpha, begin, end, direction, option), ...)
  }
}

#' @rdname scale_fish
#' @aliases scale_color_fish
#' @importFrom ggplot2 discrete_scale
#' @export
scale_colour_fish <- scale_color_fish

#' @rdname scale_fish
#' @aliases scale_color_fish
#' @export
scale_colour_fish_d <- function(option = 'Chlorurus_microrhinos', ..., alpha = 1, begin = 0, end = 1,
                              direction = 1) {
  discrete_scale("colour", "fish", fish_pal(alpha, begin, end, direction, option), ...)
}

#' @rdname scale_fish
#' @aliases scale_color_fish
#' @export
scale_color_fish_d <- scale_colour_fish_d


#' @rdname scale_fish
#' @aliases scale_fill_fish
#' @importFrom ggplot2 discrete_scale
#' @export
scale_fill_fish_d <- function(option = 'Chlorurus_microrhinos' , ..., alpha = 1, begin = 0, end = 1,
                            direction = 1) {
  discrete_scale("fill", "fish", fish_pal(alpha, begin, end, direction, option), ...)
}


#' fish colour scales
#'
#' Uses the fish color scale.
#'
#' For \code{discrete == FALSE} (the default) all other arguments are as to
#' \link[ggplot2]{scale_fill_gradientn} or \link[ggplot2]{scale_color_gradientn}.
#' Otherwise the function will return a \code{discrete_scale} with the plot-computed
#' number of colors.
#'
#'
#' @param ... parameters to \code{discrete_scale} or \code{scale_fill_gradientn}
#'
#' @param alpha pass through parameter to \code{fish}
#'
#' @param begin The (corrected) hue in [0,1] at which the fish colormap begins.
#'
#' @param end The (corrected) hue in [0,1] at which the fish colormap ends.
#'
#' @param direction Sets the order of colors in the scale. If 1, the default, colors
#' are as output by \code{fish_pal}. If -1, the order of colors is reversed.
#'
#' @param discrete generate a discrete palette? (default: \code{FALSE} - generate continuous palette)
#'
#' @param option A character string indicating the fish species to use.
#'
#' @rdname scale_fish
#'
#'
#' @importFrom ggplot2 scale_fill_gradientn scale_color_gradientn discrete_scale
#'
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' library(ggplot2)
#'
#'
#'
#' ggplot(mtcars, aes(factor(cyl), fill=factor(vs))) +
#' geom_bar() +
#' scale_fill_fish(discrete = TRUE, option = "Trimma_lantana")
#'
#' ggplot(mtcars, aes(factor(gear), fill=factor(carb))) +
#' geom_bar() +
#' scale_fill_fish(discrete = TRUE, option = "Trimma_lantana")
#'
#' ggplot(mtcars, aes(x = mpg, y = disp, colour = drat)) +
#' geom_point(size = 2) +
#' scale_colour_fish(option = "Trimma_lantana")
#'
#'
#'
#'
#' @export
scale_fill_fish <- function(option = 'Ostracion_cubicus', ..., alpha = 1, begin = 0, end = 1, direction = 1,
                          discrete = FALSE) {

  if (discrete) {
    discrete_scale("fill", "fish", fish_pal(alpha, begin, end, direction, option), ...)
  } else {
    scale_fill_gradientn(colours = fish(256, alpha, begin, end, direction, option), ...)
  }}



