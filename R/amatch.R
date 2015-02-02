#' Approximately match
#' 
#' Match x to y approximately, and return the index of y,
#' which is mostly near to each value in x.
#' A variate of match() or %in%
#' 
#' @param x A given vector to be matched
#' @param y A target vector to calculate absolute approximation
#' @export
#' @examples
#' a <- c(1,2,3)
#' b <- c(0.1, 0.2, 0.5)
#' amatch(a, b)
amatch <- function(x, y){
  sapply(x, function(i) which.min(abs(y-i)))
}

#' Sequencing by unique values
#' 
#' Generate a new integer sequence according to value groups.
#' The same value takes an unique interger.
#' 
#' @param v A vector to generate integer sequence
#' @export
#' @examples
#' seq_unique(seq(1:10))
seq_unique <- function(v) {
  v.u <- unique(v)
  sapply(v, match, v.u)
}