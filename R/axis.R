#' Rotate axis
#'
#' Rotate axis
#'
#' @param p billboard object as returned by \code{\link{b_board}}.
#'
#' @examples
#' mtcars[1:5,] %>%
#'   b_board(wt) %>%
#'   b_bar(drat) %>%
#'   b_rotate()
#'
#' @export
b_rotate <- function(p){
  opts <- list()
  opts$rotated <- TRUE
  p$x$options$axis <- append(p$x$options$axis, opts)
  p
}

#' Add axis labels
#'
#' Add axis labels.
#'
#' @param p billboard object as returned by \code{\link{b_board}}.
#' @param label axis label.
#' @param position position of label.
#' @param axis axis to add label to.
#'
#' @details
#' Valid \code{position}:
#' \itemize{
#'   \item{\code{inner-top}}
#'   \item{\code{inner-middle}}
#'   \item{\code{inner-bottom}}
#'   \item{\code{outer-top}}
#'   \item{\code{outer-middle}}
#'   \item{\code{outer-bottom}}
#' }
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_spline(cyl) %>%
#'   b_xlabel("Weight")
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") %>%
#'   b_xlabel("Model") %>%
#'   b_ylabel("Miles per galon") %>%
#'   b_ylabel("Rear axle ratio", axis = "y2")
#'
#' @rdname axis_label
#' @export
b_xlabel <- function(p, label, position = "inner-top"){
  axis_labels(p, label, position, "x")
}

#' @rdname axis_label
#' @export
b_ylabel <- function(p, label, position = "inner-top", axis = "y"){
  axis_labels(p, label, position, axis)
}

#' Customise axis
#'
#' Customise charts' axis.
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_spline(cyl) %>%
#'   b_xlabel("Weight") %>%
#'   b_ylabel("Number of cylinders") %>%
#'   b_xaxis(padding = list(left = 1, right = 1))
#'
#' @export
b_xaxis <- function(p, show = TRUE, height = NULL, min  = NULL, max = NULL, padding = NULL, ...){

  opts <- list(...)
  opts$show <- show
  if(!is.null(height)) opts$height <- height
  if(!is.null(padding)) opts$padding <- padding
  if(!is.null(min)) opts$min <- min
  if(!is.null(max)) opts$max <- max

  p$x$options$axis$x <- append(p$x$options$axis$x, opts)
  p
}

#' Customise y axis
#'
#' Cutomise y and y2 axis.
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") %>%
#'   b_xlabel("Model") %>%
#'   b_ylabel("Miles per galon") %>%
#'   b_ylabel("Rear axle ratio", axis = "y2") %>%
#'   b_yaxis("y2", inner = TRUE) %>%
#'   b_xaxis(padding = list(left = 1, right = 2)) # add padding to avoid overlap
#'
#' @export
b_yaxis <- function(p, axis = "y", show = TRUE, center = NULL, inner = FALSE, inverted = FALSE, min = NULL, max = NULL, ...){

  opts <- list(...)
  opts$show <- show
  opts$inner <- inner
  opts$inverted <- inverted
  if(!is.null(center)) opts$center <- center
  if(!is.null(min)) opts$min <- min
  if(!is.null(max)) opts$max <- max

  p$x$options$axis[[axis]] <- append(p$x$options$axis[[axis]], opts)
  p
}

#' Customise X axis ticks
#'
#' @examples
#' library(dplyr)
#'
#' mtcars %>%
#'   mutate(model = rownames(.)) %>%
#'   b_board(model) %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") %>%
#'   b_xlabel("Model") %>%
#'   b_ylabel("Miles per galon") %>%
#'   b_ylabel("Rear axle ratio", axis = "y2") %>%
#'   b_xaxis(height = 130) %>%
#'   b_xtick(rotate = 90, multiline = FALSE)
#'
#' @export
b_xtick <- function(p, centered = TRUE, count = NULL, culling = NULL, fit = TRUE, multiline = TRUE,
                    outer = TRUE, rotate = 0, width = NULL, ...){

  opts <- list(...)
  opts$centered <- centered
  opts$fit <- fit
  opts$multiline <- multiline
  opts$outer <- outer
  opts$rotate <- rotate
  if(!is.null(count)) opts$count <- count
  if(!is.null(culling)) opts$culling <- culling
  if(!is.null(width)) opts$width <- width

  p$x$options$axis$x$tick <- append(p$x$options$axis$x$tick, opts)
  p
}

#' Customise Y axis ticks
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") %>%
#'   b_ytick(count = 10) %>%
#'   b_ytick("y2", outer = FALSE)
#'
#' @export
b_ytick <- function(p, axis = "y", count = NULL, outer = TRUE){

  opts <- list()
  opts$outer <- outer
  if(!is.null(count)) opts$count <- count

  p$x$options$axis[[axis]]$tick <- append(p$x$options$axis[[axis]]$tick, opts)
  p
}
