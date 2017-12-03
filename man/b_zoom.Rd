% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{b_zoom}
\alias{b_zoom}
\title{Allow zoom}
\usage{
b_zoom(p, enable = TRUE, rescale = FALSE, ...)
}
\description{
Add zooming.
}
\examples{
mtcars \%>\%
  b_board(wt) \%>\%
  b_area(mpg) \%>\%
  b_zoom()

}