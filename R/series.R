#' Customise point
#'
#' @examples
#' mtcars %>%
#'   b_board() %>%
#'   b_line(disp) %>%
#'   b_point(r = 4)
#'
#' @export
b_point <- function(p, show = TRUE, r = 2.5, expand.enable = TRUE, expand.r = 4.5, select.r = 4, ...){

  opts <- list(...)
  opts$show <- show
  opts$r <- r
  opts$focus <- list(
    expand = list(
      enabled = expand.enable,
      r = expand.r
    )
  )
  opts$select <- list(
    r = select.r
  )

  p$x$options$point <- append(p$x$options$point, opts)
  p
}

#' Customise line
#'
#' @examples
#' df <- data.frame(x = 1:20, y = c(3, NA, runif(18, 1, 5)))
#'
#' df %>%
#'   b_board(x) %>%
#'   b_step(y) %>%
#'   b_lines(connect = TRUE, step = "step-after")
#'
#' @export
b_lines <- function(p, connect = FALSE, step = NULL, ...){

  opts <- list(...)
  opts$connect <- connect
  opts$step <- step

  p$x$options$line <- append(p$x$options$line, opts)
  p
}
