% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{b_labels}
\alias{b_labels}
\title{Customise labels}
\usage{
b_labels(p, x = -10, y = 10)
}
\arguments{
\item{p}{an object of class \code{billboard}.}

\item{x, y}{labels position.}
}
\description{
Customise labels.
}
\examples{
mtcars \%>\%
  b_board(wt) \%>\%
  b_spline(mpg) \%>\%
  b_spline(drat, axis = "y2") \%>\%
  b_labels()

}
