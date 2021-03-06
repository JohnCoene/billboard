% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{b_pad}
\alias{b_pad}
\title{Chart padding}
\usage{
b_pad(p, top = NULL, right = NULL, bottom = NULL, left = NULL, ...)
}
\arguments{
\item{p}{an object of class \code{billboard}.}

\item{top, right, bottom, left}{padding.}

\item{...}{any other parameter.}
}
\description{
Adjust chart padding.
}
\examples{
mtcars \%>\%
  b_board(wt) \%>\%
  b_area(mpg) \%>\%
  b_pad(brighten = FALSE)

}
