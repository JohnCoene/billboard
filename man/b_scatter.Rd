% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.R
\name{b_scatter}
\alias{b_scatter}
\title{Add scatter}
\usage{
b_scatter(p, serie, name = NULL, stack = FALSE, axis = "y", r = 2.5)
}
\arguments{
\item{p}{an object of class \code{billboard}.}

\item{serie}{column of values to plot.}

\item{name}{name of serie.}

\item{stack}{set to stack.}

\item{axis}{axis to plot on, see examples.}

\item{r}{point radius.}
}
\description{
Add points.
}
\examples{
mtcars \%>\%
  b_board() \%>\%
  b_scatter(mpg)

mtcars \%>\%
  b_board() \%>\%
  b_scatter(mpg, stack = TRUE) \%>\%
  b_scatter(drat, stack = TRUE)

mtcars \%>\%
  b_board() \%>\%
  b_scatter(mpg) \%>\%
  b_scatter(drat, axis = "y2")

}
