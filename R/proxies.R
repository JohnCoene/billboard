#' Zoom
#' 
#' @param an object of class \code{billboardProxy} as returned by \code{\link{billoardProxy}}.
#' @param domain domain to zoom to.
#' 
#' @examples 
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sliderInput("zoom",
#'       "Zoom on a region",
#'       min = 0,
#'       max = 100,
#'       value = 100
#'     ),
#'     mainPanel(
#'       billboardOutput("billboard")
#'     )
#'   )
#' )
#' 
#' server <- function(input, output){
#'   data <- data.frame(x = 1:100, y = runif(100, 1, 100))
#' 
#'   output$billboard <- renderBillboard({
#'     data %>% 
#'       b_board(x) %>% 
#'       b_line(y) %>% 
#'       b_zoom()
#'   })
#'   
#'   observeEvent(input$zoom, {
#'     billboardProxy("billboard") %>% 
#'     b_zoom_p(c(0, input$zoom))
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' }
#' 
#' @export
b_zoom_p <- function(chartProxy, domain){
  
  if(missing(domain))
    stop("missing domain")
  
  # prepair a message using the function arguments
  data <- list(id = chartProxy$id, domain = domain)
  
  # send a custom message to JavaScript
  chartProxy$session$sendCustomMessage("b_zoom_p", data)
  
  # return the proxy i.e. the list that holds the chart id and session object
  # so we can plug it into the next function (if any), via the pipe operator
  return(chartProxy)
}