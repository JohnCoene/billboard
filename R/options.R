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
#'   b_xaxis_label("Weight")
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") %>%
#'   b_xaxis_label("Model") %>%
#'   b_yaxis_label("Miles per galon") %>%
#'   b_yaxis_label("Rear axle ratio", axis = "y2")
#'
#' @rdname axis_label
#' @export
b_xaxis_label <- function(p, label, position = "inner-top"){
  axis_labels(p, label, position, "x")
}

#' @rdname axis_label
#' @export
b_yaxis_label <- function(p, label, position = "inner-top", axis = "y"){
  axis_labels(p, label, position, axis)
}
