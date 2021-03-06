% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.R
\name{b_bar}
\alias{b_bar}
\title{Add bar}
\usage{
b_bar(p, serie, name = NULL, stack = FALSE, axis = "y", width = list(),
  zerobased = TRUE)
}
\arguments{
\item{p}{an object of class \code{billboard}.}

\item{serie}{column of values to plot.}

\item{name}{name of serie.}

\item{stack}{set to stack.}

\item{axis}{axis to plot on, see examples.}

\item{width}{width of bars.}

\item{zerobased}{set if min or max value will be 0 on bar chart.}
}
\description{
Add a bar.
}
\examples{
mtcars \%>\%
  b_board() \%>\%
  b_bar(mpg)

mtcars \%>\%
  b_board() \%>\%
  b_bar(mpg, stack = TRUE) \%>\%
  b_bar(drat, stack = TRUE)

mtcars \%>\%
  b_board() \%>\%
  b_bar(mpg) \%>\%
  b_bar(drat, axis = "y2")

}
