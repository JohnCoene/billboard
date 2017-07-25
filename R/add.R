#' add line
#'
#' @export
b_line <- function(p, serie, name = NULL){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "line"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "line"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  p
}

#' add bar
#'
#' @export
b_bar <- function(p, serie, name = NULL){
  y <- col_dat(serie, name)

  p$x$options$data$type <- "bar"
  p$x$options$data$types <- append(p$x$options$data$types, get_type(serie, name, type = "bar"))
  p$x$options$data$columns <- append(p$x$options$data$columns, y)
  p
}
