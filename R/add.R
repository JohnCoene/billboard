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
#' @export
b_line <- function(p, serie, name = NULL, stack = FALSE){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "line"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "line"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  p
}

#' add bar
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_bar(mpg)
#'
#' @export
b_bar <- function(p, serie, name = NULL, stack = FALSE){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "bar"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "bar"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  p
}

#' add spline
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_spline(mpg)
#'
#' @export
b_spline <- function(p, serie, name = NULL, stack = FALSE){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "spline"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "spline"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  p
}

#' add step
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_step(mpg)
#'
#' @export
b_step <- function(p, serie, name = NULL, stack = FALSE){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "step"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "step"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  p
}

#' add step area
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_step_area(mpg)
#'
#' @export
b_step_area <- function(p, serie, name = NULL, stack = FALSE){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "area-step"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "area-step"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  p
}

#' add area
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_area(mpg)
#'
#' @export
b_area <- function(p, serie, name = NULL, stack = FALSE){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "area"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "area"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  p
}

#' add area spline
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_area_spline(mpg)
#'
#' @export
b_area_spline <- function(p, serie, name = NULL, stack = FALSE){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "area-spline"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "area-spline"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  p
}

#' add scatter
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_scatter(mpg)
#'
#' @export
b_scatter <- function(p, serie, name = NULL, stack = FALSE){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "scatter"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "scatter"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  if(isTRUE(stack)) p$x$options$data$groups[[1]] <- append(p$x$options$data$groups[[1]], b_stack(serie, name))
  p
}

#' add pie
#'
#' @examples
#' mtcars[1:5,] %>%
#'   b_board(rownames(.)) %>%
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
#' mtcars[1:5,] %>%
#'   b_board(rownames(.)) %>%
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
