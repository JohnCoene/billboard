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
#'
#' @export
b_board <- function(data, x, width = NULL, height = NULL, elementId = NULL) {

  if(!missing(data)){
    assign("data", data, envir = data_env)
    if(!missing(x)) assign("x", eval(substitute(x), data), envir = data_env)
  }

  # forward options using x
  x = list(
    options = list(
      data = list(
        columns = list(),
        types = list(),
        groups = list(list())
      )
    )
  )

  if (check_cat()) {
   x$options$axis <- list(
     x = list(
       type = get_cat(),
       categories = cat_x()
     )
   )
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
