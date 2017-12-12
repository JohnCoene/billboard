#' Zoom
#'
#' @param proxy an object of class \code{billboardProxy} as returned by \code{\link{billboardProxy}}.
#' @param domain domain to zoom to.
#' @param data data set.
#' @param serie,series target series.
#' @param to transformation target, see details.
#' @param axis target axis.
#' @param start,end start and end of region.
#' @param class \code{CSS} class.
#' @param x x column.
#'
#' @section Proxies:
#'
#' \describe{
#'   \item{`b_zoom_p`}{Zoom on a specific area.}
#'   \item{`b_focus_p`}{Focus on a particular serie.}
#'   \item{`b_defocus_p`}{Unfocus from particular serie.}
#'   \item{`b_transform_p`}{Change charge type.}
#'   \item{`b_stack_p`}{Stack series.}
#'   \item{`b_add_region_p`}{Add regions.}
#'   \item{`b_region_p`}{Define region.}
#'   \item{`b_flow_p`}{Add rows of data.}
#' }
#'
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(
#'     3,
#'      sliderInput("zoom",
#'        "Zoom on a region",
#'        min = 0,
#'        max = 150,
#'        value = 100
#'      )
#'     ),
#'     column(
#'       2,
#'      selectInput(
#'        "transform",
#'        "Filter:",
#'        choices = c("line", "spline", "area", "area-spline", "scatter", "bar"),
#'        selected = "line"
#'      )
#'     ),
#'     column(
#'       2,
#'      selectInput(
#'       "focus",
#'        label = "Focus on data",
#'        choices = c("y", "z"),
#'        selected = "y"
#'      )
#'     ),
#'     column(
#'       3,
#'      selectInput(
#'       "stack",
#'        label = "Stack",
#'        choices = c("x", "y", "z"),
#'        selected = "y",
#'        multiple = TRUE
#'      )
#'     ),
#'     column(
#'       2,
#'      checkboxInput(
#'        "region", "Add region", FALSE
#'      )
#'    )
#'   ),
#'   fluidRow(
#'     billboardOutput("billboard")
#'   ),
#'   fluidRow(
#'     column(
#'       3,
#'      sliderInput("add",
#'        "Add rows",
#'        min = 0,
#'        max = 100,
#'        value = 0
#'      )
#'     )
#'   )
#' )
#'
#' server <- function(input, output){
#'
#'   data <- data.frame(x = runif(100, 1, 100),
#'     y = runif(100, 1, 100),
#'     z = runif(100, 1, 100))
#'
#'   df <- reactive({
#'     data.frame(x = runif(input$add, 10, 80),
#'       y = runif(input$add, 10, 80),
#'       z = runif(input$add, 10, 80))
#'   })
#'
#'   output$billboard <- renderBillboard({
#'     data %>%
#'       b_board() %>%
#'       b_line(x) %>%
#'       b_bar(y, stack = TRUE) %>%
#'       b_area(z, stack = TRUE) %>%
#'       b_zoom()
#'   })
#'
#'   observeEvent(input$zoom, {
#'     billboardProxy("billboard") %>%
#'     b_zoom_p(c(0, input$zoom))
#'   })
#'
#'   observeEvent(input$transform, {
#'     billboardProxy("billboard") %>%
#'     b_transform_p(input$transform, "x")
#'   })
#'
#'   observeEvent(input$focus, {
#'     billboardProxy("billboard") %>%
#'     b_focus_p(list("x", input$filter))
#'   })
#'
#'   observeEvent(input$stack, {
#'     billboardProxy("billboard") %>%
#'     b_stack_p(list("x", input$stack))
#'   })
#'
#'   observeEvent(input$region, {
#'     if(isTRUE(input$region)){
#'       billboardProxy("billboard") %>%
#'       b_add_region_p(axis = "x", start = 1, end = 40)
#'     }
#'   })
#'
#'   observeEvent(input$add, {
#'     billboardProxy("billboard") %>%
#'     b_flow_p(df(), series = c("x", "y", "z"))
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @rdname proxies
#' @export
b_zoom_p <- function(proxy, domain){

  if(missing(domain))
    stop("missing domain")

  data <- list(id = proxy$id, domain = domain)

  proxy$session$sendCustomMessage("b_zoom_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
b_focus_p <- function(proxy, series){

  if(missing(series))
    stop("missing series")

  data <- list(id = proxy$id, series = series)

  proxy$session$sendCustomMessage("b_focus_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
b_defocus_p <- function(proxy, series = NULL){

  data <- list(id = proxy$id, series = series)

  proxy$session$sendCustomMessage("b_defocus_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
b_transform_p <- function(proxy, to, serie){

  if(missing(serie) || missing(to))
    stop("missing to and serie")

  data <- list(id = proxy$id, params = list(to = to, serie = serie))

  proxy$session$sendCustomMessage("b_transform_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
b_stack_p <- function(proxy, serie){

  if(missing(serie))
    stop("missing serie")

  data <- list(id = proxy$id, serie = list(serie))

  proxy$session$sendCustomMessage("b_stack_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
b_region_p <- function(proxy, axis, start, end, class = NULL){

  if(missing(axis) || missing(start) || missing(end))
    stop("missing start, end or axis")

  l <- list(axis = axis, start = start, end = end)
  if(!is.null(class)) l$class <- class

  data <- list(id = proxy$id, opts = l)

  proxy$session$sendCustomMessage("b_region_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
b_add_region_p <- function(proxy, axis, start, end, class = NULL){

  if(missing(axis) || missing(start) || missing(end))
    stop("missing start, end or axis")

  l <- list(axis = axis, start = start, end = end)
  if(!is.null(class)) l$class <- class

  data <- list(id = proxy$id, opts = l)

  proxy$session$sendCustomMessage("b_add_region_p", data)

  return(proxy)
}

#' @rdname proxies
#' @export
b_flow_p <- function(proxy, data, series, x = NULL){

  if(missing(data) || missing(series))
    stop("missing data, series")

  data <- data[, series]

  build_dat <- function(name, x){
    c(name, as.character(x))
  }

  data <- mapply(build_dat, names(data), data, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  if(!is.null(x)){
    xvar <- data[, x]
    xvar <- c("b_xAxIs", xvar)
    xvar <- append(xvar, data)
    data <- xvar
  }

  msg <- list(id = proxy$id, opts = list(columns = data))

  proxy$session$sendCustomMessage("b_flow_p", msg)

  return(proxy)
}
