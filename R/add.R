#' Add line
#'
#' Add a line.
#'
#' @inheritParams p
#' @inheritParams add_params
#' @param connect set if null data point will be connected or not.
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_line(mpg)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_line(mpg, stack = TRUE) %>%
#'   b_line(drat, stack = TRUE)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_line(mpg) %>%
#'   b_line(drat, axis = "y2")
#'
#' @export
b_line <- function(p, serie, name = NULL, stack = FALSE, axis = "y", connect = FALSE){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "line"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "line"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE

  p$x$options$line$connectNull <- connect
  p
}

#' Add bar
#'
#' Add a bar.
#'
#' @inheritParams p
#' @inheritParams add_params
#' @param width width of bars.
#' @param zerobased set if min or max value will be 0 on bar chart.
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_bar(mpg)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_bar(mpg, stack = TRUE) %>%
#'   b_bar(drat, stack = TRUE)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_bar(mpg) %>%
#'   b_bar(drat, axis = "y2")
#'
#' @export
b_bar <- function(p, serie, name = NULL, stack = FALSE, axis = "y", width = list(), zerobased = TRUE){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "bar"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "bar"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE

  if(length(width)) p$x$options$bar$width <- width
  p$x$options$bar$zerobased <- zerobased
  p
}

#' Add spline
#'
#' Add a spline.
#'
#' @inheritParams p
#' @inheritParams add_params
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_spline(mpg)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_spline(mpg, stack = TRUE) %>%
#'   b_spline(drat, stack = TRUE)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2")
#'
#' @export
b_spline <- function(p, serie, name = NULL, stack = FALSE, axis = "y"){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "spline"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "spline"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE
  p
}

#' Add step
#'
#' Add a step chart.
#'
#' @inheritParams p
#' @inheritParams add_params
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_step(mpg)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_step(mpg, stack = TRUE) %>%
#'   b_step(drat, stack = TRUE)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_step(mpg) %>%
#'   b_step(drat, axis = "y2")
#'
#' @export
b_step <- function(p, serie, name = NULL, stack = FALSE, axis = "y"){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "step"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "step"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE
  p
}

#' Add step area
#'
#' Add a step-area.
#'
#' @inheritParams p
#' @inheritParams add_params
#' @param zerobased set if min or max value will be 0 on bar chart.
#' @param above set to fill area above.
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_step_area(mpg)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_step_area(mpg, stack = TRUE) %>%
#'   b_step_area(drat, stack = TRUE)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_step_area(mpg) %>%
#'   b_step_area(drat, axis = "y2")
#'
#' @export
b_step_area <- function(p, serie, name = NULL, stack = FALSE, axis = "y", zerobased = TRUE, above = FALSE){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "area-step"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "area-step"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE

  p$x$options$area$zerobased <- zerobased
  p$x$options$area$above <- above

  p
}

#' Add area
#'
#'
#' Add an area.
#'
#' @inheritParams b_step_area
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_area(mpg)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_area(mpg, stack = TRUE) %>%
#'   b_area(drat, stack = TRUE)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_area(mpg) %>%
#'   b_area(drat, axis = "y2")
#'
#' @export
b_area <- function(p, serie, name = NULL, stack = FALSE, axis = "y", zerobased = TRUE, above = FALSE){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "area"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "area"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE

  p$x$options$area$zerobased <- zerobased
  p$x$options$area$above <- above

  p
}

#' Add area spline
#'
#' Add an area spline.
#'
#' @inheritParams p
#' @inheritParams add_params
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_area_spline(mpg)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_area_spline(mpg, stack = TRUE) %>%
#'   b_area_spline(drat, stack = TRUE)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_area_spline(mpg) %>%
#'   b_area_spline(drat, axis = "y2")
#'
#' @export
b_area_spline <- function(p, serie, name = NULL, stack = FALSE, axis = "y"){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "area-spline"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "area-spline"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE
  p
}

#' Add scatter
#'
#' Add points.
#'
#' @inheritParams p
#' @inheritParams add_params
#' @param r point radius.
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_scatter(mpg)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_scatter(mpg, stack = TRUE) %>%
#'   b_scatter(drat, stack = TRUE)
#'
#' mtcars %>%
#'   b_board() %>%
#'   b_scatter(mpg) %>%
#'   b_scatter(drat, axis = "y2")
#'
#' @export
b_scatter <- function(p, serie, name = NULL, stack = FALSE, axis = "y", r = 2.5){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "scatter"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "scatter"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE

  p$x$options$point$r <- r
  p
}


#' Add pie
#'
#' Add a pie chart.
#'
#' @inheritParams p
#' @param serie name of serie to plot.
#' @param label label parameters as list.
#' @param expand set to expand on hover.
#' @param pad.angle set padding between data.
#'
#' @examples
#' library(dplyr)
#'
#' mtcars %>%
#'   mutate(model = rownames(.)) %>%
#'   slice(1:5) %>%
#'   b_board(model) %>%
#'   b_pie(mpg)
#'
#' @export
b_pie <- function(p, serie, label = list(), expand = TRUE, pad.angle = 0){
  y <- pie_dat(serie)

  p$x$options$data$type <- "pie"
  p$x$options$data$types <- NULL
  p$x$options$data$groups <- NULL
  p$x$options$data$columns <- append(p$x$options$data$columns, y)

  if(length(label)) p$x$options$pie$label <- label
  p$x$options$pie$expand <- expand
  p$x$options$pie$padAngle <- pad.angle
  p
}

#' Add donut
#'
#' Add a donut (chart).
#'
#' @inheritParams b_pie
#' @param title donut title
#' @param width width of donut.
#'
#' @examples
#' library(dplyr)
#'
#' mtcars %>%
#'   mutate(model = rownames(.)) %>%
#'   slice(1:5) %>%
#'   b_board(model) %>%
#'   b_donut(mpg)
#'
#' @export
b_donut <- function(p, serie, label = list(), expand = TRUE, width = NULL, title = "", pad.angle = 0){
  y <- pie_dat(serie)

  p$x$options$data$type <- "donut"
  p$x$options$data$types <- NULL
  p$x$options$data$groups <- NULL
  p$x$options$data$columns <- append(p$x$options$data$columns, y)

  if(length(label)) p$x$options$donut$label <- label
  p$x$options$donut$expand <- expand
  if(!is.null(width)) p$x$options$donut$width <- width
  p$x$options$donut$title <- title
  p$x$options$donut$padAngle <- pad.angle

  p
}

#' Add gauge
#'
#' Add a gauge.
#'
#' @inheritParams p
#' @param value value to gauge.
#' @param full.circle show full circle as donut. When set to 'true',
#' the max label will not be showed due to start and end points are same location.
#' @param label label settings as list.
#' @param expand set to expand on hover.
#' @param min,max minimum and maxmum values.
#' @param start.angle gauge orientation.
#' @param units set units of gauge.
#' @param width set width of gauge.
#'
#' @examples
#' b_board() %>%
#'   b_gauge(15, units = "$")
#'
#' @export
b_gauge <- function(p, value, full.circle = FALSE, label = list(), expand = TRUE, min = 0, max = 100,
                    start.angle = (-1 * pi / 2), units = NULL, width = NULL){
  val <- list(
    c("data", value)
  )

  p$x$options$data$type <- "gauge"
  p$x$options$data$types <- NULL
  p$x$options$data$groups <- NULL
  p$x$options$data$columns <- append(p$x$options$data$columns, val)

  if(length(label)) p$x$options$gauge$label <- label
  p$x$options$gauge$fullCircle <- full.circle
  p$x$options$gauge$min <- min
  p$x$options$gauge$max <- max
  p$x$options$gauge$startingAngle <- start.angle
  if(!is.null(units)) p$x$options$gauge$units <- units
  if(!is.null(width)) p$x$options$gauge$width <- width

  p
}
