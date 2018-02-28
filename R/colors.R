#' Customise colors
#'
#' Customise colors.
#'
#' @inheritParams p
#' @inheritParams three_dots
#' @param colors colors to use.
#' @param pal \code{RColorBrewer} color palette.
#' @param n number of colors.
#' 
#' @section Colors:
#' 
#' \describe{
#'   \item{\code{b_color}}{Manual list your colors of choice.}
#'   \item{\code{b_color_brewer}}{Use palettes from the \link{RColorBrewer} package.}
#'   \item{\code{b_color_viridis}, \code{b_color_magma}, \code{b_color_plasma}}{Use palettes 
#'   from the \link{viridis} package.}
#'   \item{\code{b_color_wes}}{Use palettes from the \link{wesanderson} package}
#' }
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_area(qsec) %>%
#'   b_spline(wt) %>%
#'   b_bar(disp, axis = "y2") %>%
#'   b_step(cyl) -> p
#'
#' p %>%
#'   b_color(c("red", "blue", "yellow", "orange"))
#'
#' p %>%
#'   b_color_brewer("Dark2")
#'
#' p %>%
#'   b_color_viridis()
#'
#' p %>%
#'   b_color_magma()
#'
#' p %>%
#'   b_color_plasma()
#'   
#' p %>% 
#'   b_color_wes("Zissou1")
#'
#' @importFrom grDevices colorRampPalette
#'
#' @rdname color
#' @export
b_color <- function(p, colors, ...){

  if(missing(colors))
    stop("must pass colors", call. = FALSE)

  opts <- p$x$options$data$axes

  for(i in 1:length(p$x$options$data$axes)){
    opts[[i]] <- colors[i]
  }

  p$x$options$data$colors <- opts
  p
}

#' @rdname color
#' @export
b_color_brewer <- function(p, pal, n = 8, ...){

  if(missing(pal)) stop("must pass palette name", call. = FALSE)

  p %>%
    b_color(RColorBrewer::brewer.pal(n, pal, ...))
}

#' @rdname color
#' @export
b_color_viridis <- function(p, ...){

  viridis_colors(p, "D", ...)
}

#' @rdname color
#' @export
b_color_plasma <- function(p, ...){

  viridis_colors(p, "C", ...)
}

#' @rdname color
#' @export
b_color_inferno <- function(p, ...){

  viridis_colors(p, "B", ...)
}

#' @rdname color
#' @export
b_color_magma <- function(p, ...){

  viridis_colors(p, "A", ...)
}

#' @rdname color
#' @export
b_color_wes <- function(p, pal, ...){
  
  p %>%
    b_color(wesanderson::wes_palette(pal, ...))
  
}