build_x <- function(x){
  data <- get("data", envir = data_env)

  x <- eval(substitute(x, parent.frame()), data)
  # unique(x)
}

get_type <- function(serie, name = NULL, type){
  n <- ifelse(is.null(name), deparse(substitute(serie, parent.frame())), serie)
  types <- list(type)
  names(types) <- n
  types
}

col_dat <- function(serie, name = NULL){
  data <- get("data", envir = data_env)

  if(is.null(name)) name <- deparse(substitute(serie, parent.frame()))

  serie <- eval(substitute(serie, parent.frame()), data)

  list(c(name, serie))
}
