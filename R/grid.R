#' Customise grid
#'
#' Add and customise grid
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_bar(disp) %>%
#'   b_bar(hp, axis = "y2") -> p
#'
#' # add grid
#' p %>%
#'   b_xgrid() %>%
#'   b_ygrid() -> p
#'
#' p
#'
#' # add lines
#' p %>%
#'   b_grid_line(8, "x = 8") %>%
#'   b_grid_line(25, "x = 25") %>%
#'   b_grid_line(472, "Max disp", axis = "y")
#'
#' @rdname grid
#' @export
b_grid <- function(p, focus = TRUE, lines = TRUE){
p$x$options$grid <- list(
  focus = list(
    show = focus
  ),
  lines = list(
    front = lines
  )
)

p
}

#' @rdname grid
#' @export
b_xgrid <- function(p, show = TRUE, ...){
  set_grid(p, what = "x", show = show, ...)
}

#' @rdname grid
#' @export
b_ygrid <- function(p, show = TRUE, ...){
  set_grid(p, what = "y", show = show, ...)
}

#' @rdname grid
#' @export
b_grid_line <- function(p, value, label, axis = "x", position = NULL, class = NULL){
  if(missing(value) || missing(label))
    stop("must pass value and label", call. = FALSE)

  opts$value <- value
  opts$text <- label
  if(!is.null(position)) opts$position <- position
  if(!is.null(class)) opts$class <- class

  p$x$options$grid[[axis]]$lines <- append(p$x$options$grid[[axis]]$lines, list(opts))
  p
}
