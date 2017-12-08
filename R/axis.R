#' Rotate axis
#'
#' Rotate axis
#'
#' @inheritParams p
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
#' @param axis axis to plot on.
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
#' @inheritParams p
#' @inheritParams three_dots
#' @param show set to show axis.
#' @param height height of axis.
#' @param min,max minimum and maximum.
#' @param padding axis padding.
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
#' @inheritParams p
#' @inheritParams three_dots
#' @param axis axis to customise.
#' @param show set to show.
#' @param center customise center.
#' @param inner set to show inside chart.
#' @param inverted set to invert axis.
#' @param min,max minimum and maximum.
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
#' Customise X axis ticks.
#'
#' @inheritParams p
#' @inheritParams three_dots
#' @param centered set to center.
#' @param count set to count.
#' @param culling set to trim.
#' @param fit set to fit.
#' @param multiline set to spread on multiple lines.
#' @param outer set to position outside chart.
#' @param rotate degree.
#' @param width width of axis.
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
#' Customise Y axis ticks.
#'
#' @inheritParams p
#' @param axis target axis.
#' @param count set to count.
#' @param outer set to position outside chart.
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

#' Define X axis
#'
#' Define X axis, pipe it after all series.
#'
#' @inheritParams p
#'
#' @examples
#' library(dplyr)
#' 
#' # this makes no sense, it's just an example.
#' mtcars %>%
#'   #' mtcars %>%
#'   group_by(wt) %>%
#'   summarise(
#'     mpg = sum(mpg),
#'     drat = mean(drat)
#'   ) %>%  
#'   b_board(wt) %>%
#'   b_line(mpg) %>%
#'   b_line(drat) %>%
#'   b_setx()
#'   
#' @details pipe if after all series.
#'
#' @export
b_setx <- function(p){
  
  opts <- list()
  y <- get("x", envir = data_env)
  y <- list(c("b_xaxis", as.character(y)))
  
  series <- names(p$x$options$data$types)
  xs <- as.list(rep("b_xaxis", length(series)))
  names(xs) <- series

  p$x$options$data$xs <- xs
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  p
}

