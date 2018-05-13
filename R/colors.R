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
#'   \item{\code{b_color_brewer}}{Use palettes from the \link[RColorBrewer]{brewer.pal} package.}
#'   \item{\code{b_color_viridis}}{Use palettes from the \link[viridis]{viridis} package.}
#'   \item{\code{b_color_wes}}{Use palettes from the \link[wesanderson]{wes_palette} package}
#'   \item{\code{b_color_ghibli}}{Use palettes from the \link[ghibli]{ghibli_palette} package}
#' }
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_area(qsec) %>%
#'   b_spline(wt) %>%
#'   b_bar(disp, axis = "y2") %>%
#'   b_step_area(cyl) -> p
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
#'   b_color_wes("Zissou1")
#'   
#' p %>% 
#'   b_color_ghibli("PonyoMedium")
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
b_color_viridis <- function(p, pal = "D", ...){

  viridis_colors(p, pal, ...)
}

#' @rdname color
#' @export
b_color_plasma <- function(p, ...){
  
  .Deprecated("b_color_viridis")

  viridis_colors(p, "C", ...)
}

#' @rdname color
#' @export
b_color_inferno <- function(p, ...){
  
  .Deprecated("b_color_viridis")

  viridis_colors(p, "B", ...)
}

#' @rdname color
#' @export
b_color_magma <- function(p, ...){
  
  .Deprecated("b_color_viridis")

  viridis_colors(p, "A", ...)
}

#' @rdname color
#' @export
b_color_wes <- function(p, pal, ...){
  
  if(missing(pal))
    stop("must pass pal", call. = FALSE)
  
  p %>%
    b_color(wesanderson::wes_palette(pal, ...))
  
}

#' @rdname color
#' @export
b_color_ghibli <- function(p, pal, ...){
  
  if(missing(pal))
    stop("must pass pal", call. = FALSE)
  
  p %>%
    b_color(ghibli::ghibli_palette(pal, ...))
  
}