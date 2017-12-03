#' Show labels
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") %>%
#'   b_labels()
#'
#' @export
b_labels <- function(p, x = -10, y = 10){
  p$x$options$data$labels <- TRUE
  p$x$options$data$labels <- list(
    position = list(
      x = x,
      y = y
    )
  )
  p
}

#' Add tooltip
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") %>%
#'   b_tooltip(grouped = TRUE)
#'
#' @export
b_tooltip <- function(p, show = TRUE, grouped = FALSE, order = "asc", ...){

  opts <- list(...)
  opts$show <- show
  opts$grouped <- grouped
  opts$order <- order

  p$x$options$tooltip <- opts

  p
}

#' Add subchart
#'
#' Add a subchart.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_area(mpg) %>%
#'   b_subchart()
#'
#' @export
b_subchart <- function(p, show = TRUE, height = 20, ...){

  opts <- list(...)
  opts$show <- show
  opts$size <- list(height = height)

  p$x$options$subchart <- opts
  p
}

#' Allow zoom
#'
#' Add zooming.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_area(mpg) %>%
#'   b_zoom()
#'
#' @export
b_zoom <- function(p, enable = TRUE, rescale = FALSE, ...){

  opts <- list(...)
  opts$enabled <- enable
  opts$rescale <- rescale

  p$x$options$zoom <- opts
  p
}
