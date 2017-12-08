get_cat <- function(){
  x <- tryCatch(get("x", envir = data_env), error = function(e) e)
  
  return <- "value"

  if(!is(x, "error")){
    if(inherits(x, "factor") || inherits(x, "character")){
      return <- "category"
    } else if(inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")){
      return <- "timeseries"
    }
  } 
  
  return
}

get_format <- function(){
  x <- tryCatch(get("x", envir = data_env), error = function(e) e)
  
  format <- NULL
  
  if(!is(x, "error")){
    if(inherits(x, "Date")){
      x <- as.Date(x, format = "%Y-%m-%d")
      format <- "%Y-%m-%d"
    } else if(inherits(x, "POSIXct") || inherits(x, "POSIXlt")){
      x <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")
      format <- "%Y-%m-%d %H:%M:%S"
    }
    assign("x", x, envir = data_env)
  }
  
  format
}

check_cat <- function(){
  ifelse(get_cat() == "category", TRUE, FALSE)
}

check_time <- function(){
  ifelse(get_cat() == "timeseries", TRUE, FALSE)
}

cat_x <- function(){
  x <- get("x", envir = data_env)
  x <- unique(x)
  as.list(x)
}

get_type <- function(serie, name = NULL, type){
  n <- ifelse(is.null(name), deparse(substitute(serie, parent.frame())), name)
  types <- list(type)
  names(types) <- n
  types
}

set_axes <- function(serie, name = NULL, which){
  n <- ifelse(is.null(name), deparse(substitute(serie, parent.frame())), name)
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

setx_dat <- function(serie){
  data <- get("data", envir = data_env)
  
  serie <- eval(substitute(serie, parent.frame()), data)
  
  list(serie)
}

b_stack <- function(serie, name){
  if(is.null(name)) name <- deparse(substitute(serie, parent.frame()))
  name
}

pie_dat <- function(serie){
  data <- get("data", envir = data_env)
  x <- get("x", envir = data_env)

  serie <- eval(substitute(serie, parent.frame()), data)

  sapply(1:length(serie), function(i, x, serie){
    list(c(x[i], serie[i]))
  }, x, serie)

}

axis_labels <- function(p, text, position = "outer-center", what = "x"){
  if(missing(text)) stop("must pass text", call. = FALSE)

  opts <- list()
  opts$text <- text
  opts$position <- position
  p$x$options$axis[[what]]$label <- append(p$x$options$axis[[what]]$label, opts)
  p
}

viridis_colors <- function(p, palette = "D", ...){
  p %>%
    b_colors(colorRampPalette(viridis::viridis(length(p$x$options$data$axes),
                                               option = palette, ...))(length(p$x$options$data$axes)))
}

set_grid <- function(p, what = "x", show = TRUE, ...){
  opts <- list(...)
  opts$show <- show

  p$x$options$grid[[what]] <- append(p$x$options$grid[[what]], opts)
  p
}
