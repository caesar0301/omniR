% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/amatch.R
\name{seq_unique}
\alias{seq_unique}
\title{Sequencing by unique values}
\usage{
seq_unique(v)
}
\arguments{
\item{v}{A vector to generate integer sequence}
}
\description{
Generate a new integer sequence according to value groups.
The same value takes an unique interger.
}
\examples{
seq_unique(seq(1:10))
}

