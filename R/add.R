#' add line
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
b_line <- function(p, serie, name = NULL, stack = FALSE, axis = "y"){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "line"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "line"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE
  p
}

#' add bar
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
b_bar <- function(p, serie, name = NULL, stack = FALSE, axis = "y"){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "bar"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "bar"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE
  p
}

#' add spline
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

#' add step
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

#' add step area
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
b_step_area <- function(p, serie, name = NULL, stack = FALSE, axis = "y"){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "area-step"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "area-step"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE
  p
}

#' add area
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
b_area <- function(p, serie, name = NULL, stack = FALSE, axis = "y"){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "area"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "area"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE
  p
}

#' add area spline
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

#' add scatter
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
b_scatter <- function(p, serie, name = NULL, stack = FALSE, axis = "y"){

  if(!axis %in% c("y", "y2")) stop("axis must be y or y2", call. = FALSE)
  y <- col_dat(serie, name)

  p$x$options$data$type <- "scatter"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "scatter"))
  p$x$options$data$axes <- append(p$x$options$data$axes, set_axes(serie, name, which = axis))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  if(axis == "y2") p$x$options$axis$y2$show <- TRUE
  p
}

#' add pie
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
b_pie <- function(p, serie){
  y <- pie_dat(serie)

  p$x$options$data$type <- "pie"
  p$x$options$data$types <- NULL
  p$x$options$data$groups <- NULL
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  p
}

#' add donut
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
b_donut <- function(p, serie){
  y <- pie_dat(serie)

  p$x$options$data$type <- "donut"
  p$x$options$data$types <- NULL
  p$x$options$data$groups <- NULL
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  p
}

#' add gauge
#'
#' @examples
#' b_board() %>%
#'   b_gauge(15)
#'
#' @export
b_gauge <- function(p, value){
  val <- list(
    c("data", value)
  )

  p$x$options$data$type <- "gauge"
  p$x$options$data$types <- NULL
  p$x$options$data$groups <- NULL
  p$x$options$data$columns <- append(p$x$options$data$columns, val)
  p
}
