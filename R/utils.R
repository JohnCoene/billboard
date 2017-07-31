build_x <- function(x){
  data <- get("data", envir = data_env)

  x <- eval(substitute(x, parent.frame()), data)
  list(c("x", x))
}

get_type <- function(serie, name = NULL, type){
  n <- ifelse(is.null(name), deparse(substitute(serie, parent.frame())), serie)
  types <- list(type)
  names(types) <- n
  types
}

set_axes <- function(serie, name = NULL, which){
  n <- ifelse(is.null(name), deparse(substitute(serie, parent.frame())), serie)
  axis <- list(which)
  names(axis) <- n
  axis
}

col_dat <- function(serie, name = NULL){
  data <- get("data", envir = data_env)

  if(is.null(name)) name <- deparse(substitute(serie, parent.frame()))

  serie <- eval(substitute(serie, parent.frame()), data)

  list(c(name, serie))
}

b_stack <- function(serie, name){
  if(is.null(name)) name <- deparse(substitute(serie, parent.frame()))
  name
}

pie_dat <- function(serie){
  data <- get("data", envir = data_env)
  x <- get("x", envir = data_env)
  x <- eval(substitute(x), data)

  serie <- eval(substitute(serie, parent.frame()), data)

  sapply(1:length(serie), function(i, x, serie){
    list(c(x[i], serie[i]))
  }, x, serie)

}
