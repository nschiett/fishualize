#' Available fish silhouettes
#'
#' This function returns a dataframe containing the all the available fish
#' silhouettes accessible through the 'fishualize' package.
#'
#' @return \code{fishapes} returns a dataframe containing the all the available fish
#' silhouettes available to use.
#'
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom httr content
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split
#' @importFrom stringr str_subset
#' @importFrom stringr str_sub
#' @importFrom stringr str_replace
#' @importFrom tidyr separate
#' @importFrom dplyr mutate
#'
#'
#' @examples
#' fishapes()
#'
#' @rdname fishapes
#' @export



fishapes <- function(){


  req <- httr::GET("https://github.com/simonjbrandl/fishape/tree/master/shapes")
  httr::stop_for_status(req)
  text <- httr::content(req, "text")

  text_sub <- stringr::str_split(text, "\\s+")[[1]] %>%
    stringr::str_subset(".png")  %>%
    stringr::str_subset("title") %>%
    stringr::str_sub(start = 8, end = -6)

  df <- data.frame(text_sub = text_sub) %>%
    tidyr::separate(text_sub, into = c("family", "option"), sep = "_") %>%
    dplyr::mutate(option = stringr::str_replace(option, "[.]", "_"))

  return(df)
}





#' fish silhouette in ggplot2
#'
#' Adds a fish silhouette to your plot
#'
#' @param family A character string indicating the fish family to use.
#' @param option A character string indicating the fish species to use.
#' If NA the first avalable option within a family will be used
#' @param xmin x location giving horizontal location of raster
#' @param xmax x location giving horizontal location of raster
#' @param ymin y location giving vertical location of raster
#' @param ymax y location giving vertical location of raster
#' @param absolute logical parameter stating whether or not locations of
#' coordinates are provided in absolute data coordinates.
#' If set to FALSE, the locations of the x and y coordinates
#' should range between 0 and 1.
#' @param xlim,ylim vectors of length = 2, containing the limits of the data.
#' These have to be provided if absolute is set to FALSE.
#' @param fill color of fish shape
#' @param alpha transparancy of fish shape (should range between 0 and 1)
#'
#' @rdname add_fishape
#'
#'
#' @importFrom ggplot2 annotation_custom
#' @importFrom grid rasterGrob
#' @importFrom png readPNG
#' @importFrom scales alpha
#'
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot() + add_fishape(fill = fish(n = 5)[4])
#'
#' ggplot(diamonds)+
#'   geom_bar(aes(cut, fill = cut)) +
#'   scale_fill_fish_d(option = "Naso_lituratus") +
#'   add_fishape(family = "Acanthuridae",
#'               option = "Naso_unicornis",
#'               xmin = 1, xmax = 3, ymin = 15000, ymax = 20000,
#'               fill = fish(option = "Naso_lituratus", n = 4)[2],
#'               alpha = 0.8) +
#'   theme_bw()
#'
#' ## example with relative coordinates
#' ggplot(diamonds)+
#'   geom_bar(aes(cut, fill = cut)) +
#'   scale_fill_fish_d(option = "Naso_lituratus") +
#'   add_fishape(family = "Acanthuridae",
#'               option = "Naso_unicornis",
#'               xmin = 0, xmax = 0.3, ymin = 0.8, ymax = 1,
#'               absolute = FALSE,
#'               xlim = c(0.5, 5.5), ylim = c(0, 21000) ,
#'               fill = fish(option = "Naso_lituratus", n = 5)[3],
#'               alpha = 1) +
#'   theme_bw()
#'
#' @export
#'
add_fishape <- function(family = "Pomacanthidae",
                        option = "Centropyge_loricula",
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                        absolute = TRUE,
                        xlim = NULL, ylim = NULL,
                        fill = "#000000",
                        alpha = 1){

  # check if shape is available

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

  # get shape

  url <- paste0(
    "https://raw.githubusercontent.com/simonjbrandl/fishape/master/shapes/",
    family, "_", gsub("_", ".", option), ".png")

  img <- wrap.url(url, png::readPNG)
  g <- grid::rasterGrob(img, interpolate=TRUE)

  # reset color and alpha
  oldcol <- names(sort(table(g$raster), decreasing=TRUE))
  oldcol <- oldcol[!oldcol == "#FFFFFF00"]
  oldcol <- oldcol[1]

  newcol <- scales::alpha(fill, alpha)
  g$raster[g$raster != oldcol] <- NA
  g$raster[g$raster == oldcol] <- newcol

  # possibility to rescale
  if (absolute == FALSE){
    ## first check if all info is there
    if(xmin < 0 | xmax > 1 | ymin < 0 | ymax > 1 |
       is.null(xlim) | is.null(ylim)){
      stop("If absolute = FALSE,
           min and max of x and y gave to lie between 0 and 1 and
           xlim and ylim have to be defined")
    }
    ## rescale to absolute
    xmin2 <- ((xlim[2] - xlim[1]) * xmin) + xlim[1]
    xmax2 <- ((xlim[2] - xlim[1]) * xmax) + xlim[1]
    ymin2 <- ((ylim[2] - ylim[1]) * ymin) + ylim[1]
    ymax2 <- ((ylim[2] - ylim[1]) * ymax) + ylim[1]

    ggplot2::annotation_custom(g, xmin = xmin2, xmax = xmax2,
                               ymin = ymin2, ymax = ymax2)

  } else{
    ggplot2::annotation_custom(g, xmin = xmin, xmax = xmax,
                               ymin = ymin, ymax = ymax)
  }

}

###### function borrowed from r package imager #####
wrap.url <- function(file,fun)
{
  is.url <- grepl("^(http|ftp)s?://", file)
  if (is.url)
  {
    url <- file
    ext <- stringr::str_extract_all(url,"\\.([A-Za-z0-9]+$)")[[1]]
    if (length(ext) > 0) file <- tempfile(fileext=ext) else file <- tempfile()
    downloader::download(url,file,mode="wb")
    out <- fun(file)
    unlink(file)
    out
  }
  else
  {
    if (!utils::file_test("-f",file))
    {
      stop("File not found")
    }
    else
    {
      fun(file)
    }
  }

}




