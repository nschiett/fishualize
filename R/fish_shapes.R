#' fish silhouette in ggplot2
#'
#' Adds a fish silhouette to your plot
#'
#' @param family A character string indicating the fish family to use.
#' @param option A character string indicating the fish species to use.
#' If NA the first avalable option within a family will be used
#' @param xmin x location (in data coordinates) giving horizontal location of raster
#' @param xmax x location (in data coordinates) giving horizontal location of raster
#' @param ymin y location (in data coordinates) giving vertical location of raster
#' @param ymax y location (in data coordinates) giving vertical location of raster
#'
#'
#' @rdname add_fishape
#'
#'
#' @importFrom ggplot2 annotation_custom
#' @importFrom grid rasterGrob
#' @importFrom imager load.image
#'
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot() + add_fishape(fill = "#2f87e4")
#'
#' ggplot(diamonds)+
#'   geom_bar(aes(cut, fill = cut)) +
#'   scale_fill_fish_d(option = "Naso_lituratus") +
#'   add_fishape(family = "Acanthuridae",
#'               option = "Naso_unicornis",
#'               xmin = 1, xmax = 3, ymin = 15000, ymax = 20000,
#'               fill = fish(option = "Naso_lituratus", n = 4)[2]) +
#'   theme_bw()
#'
#' @export
#'
add_fishape <- function(family = "Acanthuridae", option = NA, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#000000"){

  shapes <- fishapes()

  if (is.na(option)){
    option <- shapes[shapes$family == family, "option"][1]
  }

  if (nrow(shapes[shapes$family == family, ]) == 0){
    stop("This family is not available or misspelled")
  }

  if (nrow(shapes[shapes$option == option, ]) == 0){
    stop("This species option is not available or misspelled")
  }

  url <- paste0(
    "https://raw.githubusercontent.com/simonjbrandl/fishape/master/shapes/",
    family, "_", gsub("_", ".", option), ".png")

  img <- imager::load.image(url)
  g <- grid::rasterGrob(img, interpolate=TRUE)
  oldcol <- names(sort(table(g$raster), decreasing=TRUE)[1])
  newcol <- fill
  g$raster[g$raster == oldcol] <- newcol
  g$raster[g$raster == "#FFFFFF"] <- NA
  ggplot2::annotation_custom(g, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
