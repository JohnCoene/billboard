#' Create billboard object
#'
#' Create billboard chart.
#'
#' @param data data.frame containing data to plot.
#' @param x variable column.
#' @param width,height dimensions of chart.
#' @param elementId id of div containing chart.
#'
#' @import htmlwidgets
#' @importFrom methods is
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_line(wt)
#'
#' @export
b_board <- function(data, x, width = "100%", height = NULL, elementId = NULL) {
  
  if(!missing(x))
    xp <- eval(substitute(x), data)
    
  # forward options using x
  x <- list(
    options = list(
      data = list(
        columns = list(),
        types = list(),
        groups = list(list())
      )
    )
  )
  
  if(!missing(data)){
    x$dataOrig <- data
    if(exists("xp")){
      x$xOrig <- xp
      
      if (check_cat(xp)) {
        x$options$axis <- list(
          x = list(
            type = get_cat(xp),
            categories = cat_x(xp)
          )
        )
      }
      
      x$options$data$x <- "b_xAxIs"
      if(check_time(xp)){
        x$options$axis$x$type <- "timeseries"
        x$options$axis$x$localtime <- FALSE
        x$options$data$xFormat <- get_format(xp)
        x$options$axis$x$tick <- list(
          format = get_format(xp)
        )
      }
      
      x$options$data$columns[[1]] <- c("b_xAxIs", as.character(xp))
    }
  }
  
  # create widget
  htmlwidgets::createWidget(
    name = 'billboard',
    x,
    width = width,
    height = height,
    package = 'billboard',
    elementId = elementId
  )
}

#' Shiny bindings for billboard
#'
#' Output and render functions for using billboard within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a billboard
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param id target chart id.
#' @param session shiny session
#'
#' @name billboard-shiny
#'
#' @export
billboardOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'billboard', width, height, package = 'billboard')
}

#' @rdname billboard-shiny
#' @export
renderBillboard <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, billboardOutput, env, quoted = TRUE)
}

#' @rdname billboard-shiny
#' @export
billboardProxy <- function(id, session = shiny::getDefaultReactiveDomain()){

  proxy <- list(id = id, session = session)
  class(proxy) <- "billboardProxy"

  return(proxy)
}
