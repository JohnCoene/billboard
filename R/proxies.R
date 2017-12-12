#' Zoom
#' 
#' @param proxy an object of class \code{billboardProxy} as returned by \code{\link{billoardProxy}}.
#' @param domain domain to zoom to.
#' 
#' @examples 
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   fluidRow(
#'     column(
#'     3,
#'      sliderInput("zoom",
#'        "Zoom on a region",
#'        min = 0,
#'        max = 100,
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
#'        choices = c("y", "z"),
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
#'   )
#' )
#' 
#' server <- function(input, output){
#'   data <- data.frame(x = runif(100, 1, 100), 
#'     y = runif(100, 1, 100), 
#'     z = runif(100, 1, 100))
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