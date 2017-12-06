#' Customise labels
#'
#' Customise labels.
#'
#' @inheritParams p
#' @param x,y labels position.
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
#' Add a tooltip.
#'
#' @inheritParams p
#' @inheritParams three_dots
#' @param show set to show tooltip.
#' @param grouped set if tooltip is grouped or not for the data points.
#' @param order set tooltip data display order.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") %>%
#'   b_tooltip(grouped = TRUE) %>%
#'   b_xgrid(show = FALSE) %>%
#'   b_grid_line(14, "x = 14")
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
#' @inheritParams p
#' @inheritParams three_dots
#' @param show set to show subchart.
#' @param height height of subchart.
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
#' @inheritParams p
#' @inheritParams three_dots
#' @param enable set to enable.
#' @param rescale set to rescale.
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

#' Chart interactions
#'
#' Chart interactions.
#'
#' @inheritParams p
#' @inheritParams three_dots
#' @param enabled set to enable.
#' @param brighten set to brighten.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_area(mpg) %>%
#'   b_inter(brighten = FALSE)
#'
#' @export
b_inter <- function(p, enabled = TRUE, brighten = TRUE, ...){

  opts <- list(...)
  opts$brighten <- brighten
  opts$enabled <- enabled

  p$x$options$interaction <- opts
  p
}

#' Chart padding
#'
#' Adjust chart padding.
#'
#' @inheritParams p
#' @inheritParams three_dots
#' @param top,right,bottom,left padding.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_area(mpg) %>%
#'   b_pad(brighten = FALSE)
#'
#' @export
b_pad <- function(p, top = NULL, right = NULL, bottom = NULL, left = NULL, ...){

  opts <- list(...)
  if(!is.null(top)) opts$top <- top
  if(!is.null(right)) opts$right <- right
  if(!is.null(bottom)) opts$bottom <- bottom
  if(!is.null(left)) opts$left <- left

  p$x$options$padding <- opts

  p
}

#' Add regions
#'
#' Add regions to chart.
#'
#' @inheritParams p
#' @inheritParams three_dots
#' @param axis axis to draw region on.
#' @param start,end start and end of region.
#' @param class class of region (\code{CSS}).
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_scatter(mpg) %>%
#'   b_region(axis = "x", start = 8, end = 20) %>%
#'   b_region(axis = "x", start = 1, end = 4) %>%
#'   b_region(axis = "y", start = 30, end = 34)
#'
#' @export
b_region <- function(p, axis, start, end, class = NULL, ...){

  if(missing(axis) || missing(start) || missing(end))
    stop("must pass axis, start and end.", call. = FALSE)

  opts <- list(...)
  opts$axis <- axis
  opts$start <- start
  opts$end <- end
  if(!is.null(class)) opts$class <- class

  p$x$options$regions <- append(p$x$options$regions, list(opts))

  p
}


#' Resize
#'
#' Chart resize.
#'
#' @inheritParams p
#' @param auto set to automatically resize chart.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_scatter(mpg) %>%
#'   b_resize(FALSE)
#'
#' @export
b_resize <- function(p, auto = TRUE){

  p$x$options$resize <- list(auto = auto)

  p
}

#' Size
#'
#' Chart size.
#'
#' @inheritParams p
#' @param width,height dimensions.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_scatter(mpg) %>%
#'   b_size(900, 500)
#'
#' @export
b_size <- function(p, width = NULL, height = NULL){

  if(!is.null(width)) p$x$options$size$width <- width
  if(!is.null(height)) p$x$options$size$height <- height

  p
}

#' Add svg class
#'
#' Add class to svg.
#'
#' @inheritParams p
#' @param class \code{CSS class}.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_scatter(mpg) %>%
#'   b_svg("svg-class")
#'
#' @export
b_svg <- function(p, class){

  if(missing(class)) stop("must pass class", call. = FALSE)

  p$x$options$svg <- list(classname = class)

  p
}

#' Add title
#'
#' Add a title to the chart.
#'
#' @inheritParams p
#' @inheritParams three_dots
#' @param title chart title.
#' @param position title position.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_scatter(mpg) %>%
#'   b_title("Chart title", position = "top-right")
#'
#' @export
b_title <- function(p, title = NULL, position = "top-center", ...){

  opts <- list(...)
  opts$position <- position
  if(!is.null(title)) opts$text <- title

  p$x$options$title <- opts

  p
}

#' Cutomise transition
#'
#' Set duration of transition for chart animation.
#'
#' @inheritParams p
#' @param duration duration of animations.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_scatter(mpg) %>%
#'   b_trans(1000)
#'
#' @export
b_trans <- function(p, duration = 350){

  p$x$options$transition <- list(duration)

  p
}
