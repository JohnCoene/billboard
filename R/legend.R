#' Customise legend
#'
#' @inheritParams p
#' @inheritParams three_dots
#' @param show set to show.
#' @param position legend position, see details.
#' @param equally set to all items have same width size.
#' @param padding legend padding.
#' @param width,height dimensions.
#' 
#' @details Valid \code{position}:
#' \itemize{
#'   \item{\code{bottom}}
#'   \item{\code{right}}
#'   \item{\code{inset}}
#' }
#'
#' @examples
#' mtcars %>%
#'   b_board(wt) %>%
#'   b_spline(mpg) %>%
#'   b_spline(drat, axis = "y2") -> p
#'
#' p %>%
#'   b_legend(width = 25, height = 20)
#'
#' p %>%
#'   b_legend(FALSE)
#'
#' @export
b_legend <- function(p, show = TRUE, position = "bottom", equally = FALSE, padding = 0,
                     width = 10, height = 10, ...){
  
  if(!position %in% c("inset", "right", "bottom"))
    stop("invalid position.")

  opts <- list(...)
  opts$show <- show
  opts$position <- position
  opts$equally <- equally
  opts$padding <- padding

  p$x$options$legend <- opts

  tile <- list(
    width = width,
    height = height
  )

  p$x$options$legend$item$tile <- tile

  p
}
