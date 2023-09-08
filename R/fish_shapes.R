try_GET <- function(x, ...) {
  tryCatch(
    httr::GET(url = x, httr::timeout(10), ...),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
}
is_response <- function(x) {
  class(x) == "response"
}


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
#' @importFrom curl has_internet
#' @importFrom httr message_for_status
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split
#' @importFrom stringr str_subset
#' @importFrom stringr str_sub
#' @importFrom stringr str_replace
#' @importFrom tidyr separate
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#'
#' @examples
#' fishapes()
#'
#' @rdname fishapes
#' @export



fishapes <- function(){

  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  url <- "https://github.com/simonjbrandl/fishape/tree/master/shapes"

  # Then try for timeout problems
  resp <- try_GET(url)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }

  # if status code is not 200 (ok) return null
  if(!resp$status_code == 200) {
    message(paste("Status code of http request is", resp$status_code, sep = " "))
    return(invisible(NULL))
  }

  req <- httr::GET(url)

  httr::stop_for_status(req)
  text <- httr::content(req, "text")

  text_sub <- stringr::str_split(text, "\\s+")[[1]] %>%
    stringr::str_subset(".png")  %>%
    stringr::str_subset("title") %>%
    stringr::str_sub(start = 8, end = -6)

  df <- data.frame(text_sub = text_sub) %>%
    tidyr::separate(text_sub, into = c("family", "option"), sep = "_") %>%
    dplyr::mutate(option = stringr::str_replace(.data$option, "[.]", "_"))

  df
}





#' fish silhouette in ggplot2
#'
#' Adds a fish silhouette to your plot
#'
#' @param family character string indicating the fish family.
#' @param option character string indicating the fish species.
#' If NA, the first available option within a family will be selected
#' @param xmin x location giving minimum horizontal location of silhouette
#' @param xmax x location giving maximum horizontal location of silhouette
#' @param ymin y location giving minimum vertical location of silhouette
#' @param ymax y location giving maximum vertical location of silhouette
#' @param scaled logical parameter. If TRUE, location parameters
#' (xmin, xmax, ymin, ymax) should range between 0 and 1.
#' If FALSE, location parameters should be provided according to the values on
#' the plot axes.
#' @param xlim,ylim vectors of length = 2, contains the data limits
#' and must be provided if scaled is TRUE.
#' @param fill color of fish shape
#' @param alpha transparency of fish shape (value between 0 and 1)
#'
#' @rdname add_fishape
#'
#' @return Adds a fish silhouette grob to a ggplot object.
#'
#'
#' @importFrom ggplot2 annotation_custom
#' @importFrom grid rasterGrob
#' @importFrom png readPNG
#' @importFrom scales alpha
#' @importFrom httr GET
#' @importFrom httr message_for_status
#' @importFrom curl has_internet
#'
#'
#' @examples
#' \dontrun{
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
#'               fill = fish(option = "Naso_lituratus", n = 5)[3],
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
#'               scaled = TRUE,
#'               xlim = c(0.5, 5.5), ylim = c(0, 21000) ,
#'               fill = fish(option = "Naso_lituratus", n = 5)[3],
#'               alpha = 1) +
#'   theme_bw()
#' }
#' @export
#'
add_fishape <- function(family = "Pomacanthidae",
                        option = "Centropyge_loricula",
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                        scaled = FALSE,
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

  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(10), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  is_response <- function(x) {
    class(x) == "response"
  }

  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(url)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }

  # if status code is not 200 (ok) return null
  if(!resp$status_code == 200) {
    message(paste("Status code of http request is", resp$status_code, sep = " "))
    return(invisible(NULL))
  }


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
  if (scaled == TRUE){
    ## first check if all info is there
    if(xmin < 0 | xmax > 1 | ymin < 0 | ymax > 1 |
       is.null(xlim) | is.null(ylim)){
      stop("If scaled = TRUE,
           min and max of x and y have to lie between 0 and 1 and
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




