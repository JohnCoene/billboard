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
#' @examples
#' mtcars %>%
#'   b_board(mpg) %>%
#'   b_area(qsec) %>%
#'   b_spline(wt) %>%
#'   b_bar(disp, axis = "y2") %>%
#'   b_step(cyl) -> p
#'
#' p %>%
#'   b_colors(c("red", "blue", "yellow", "orange"))
#'
#' p %>%
#'   b_brewer("Dark2")
#'
#' p %>%
#'   b_viridis()
#'
#' p %>%
#'   b_magma()
#'
#' p %>%
#'   b_plasma()
#'
#' @importFrom grDevices colorRampPalette
#'
#' @rdname color
#' @export
b_colors <- function(p, colors, ...){

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
b_brewer <- function(p, pal, n = 8, ...){

  if(missing(pal)) stop("must pass palette name", call. = FALSE)

  p %>%
    b_colors(RColorBrewer::brewer.pal(n, pal, ...))
}

#' @rdname color
#' @export
b_viridis <- function(p, ...){

  viridis_colors(p, "D", ...)
}

#' @rdname color
#' @export
b_plasma <- function(p, ...){

  viridis_colors(p, "C", ...)
}

#' @rdname color
#' @export
b_inferno <- function(p, ...){

  viridis_colors(p, "B", ...)
}

#' @rdname color
#' @export
b_magma <- function(p, ...){

  viridis_colors(p, "A", ...)
}
