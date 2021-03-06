% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/amatch.R
\name{amatch}
\alias{amatch}
\title{Approximately match}
\usage{
amatch(x, y)
}
\arguments{
\item{x}{A given vector to be matched}

\item{y}{A target vector to calculate absolute approximation}
}
\description{
Match x to y approximately, and return the index of y,
which is mostly near to each value in x.
A variate of match() or %in%
}
\examples{
a <- c(1,2,3)
b <- c(0.1, 0.2, 0.5)
amatch(a, b)
}

