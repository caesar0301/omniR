#' Bined vector
#' 
#' Bin a vector into n intervals in regard with its value range.
#' The vector x is split into n bins within [min(x), max(x)],
#' and bin index is given by checking the bin [bin_min, bin_max)
#' into which data points in x fall.
#' 
#' @param x A vector to bin
#' @param n Number of bins
#' @param type Interval representation form, c("i", "c")
#' @return Interval index if type is "i"; interval center point if
#'     type is "c".
#' @export
#' @family converting functions
#' @examples
#' vbin(seq(1:10), 3)
vbin <- function(x, n, type="i"){
  x.bin <- seq(floor(min(x)), ceiling(max(x)), length.out=n+1)
  x.int <- findInterval(x, x.bin, all.inside = TRUE)
  if(type == "i")
    return(x.int)
  else if (type == "c")
    return(0.5 * (x.bin[x.int] + x.bin[x.int+1]))
  else
    stop("Invalid type argument: type=c('i','c')")
}

#' Bined range
#' 
#' Bin the range of given vector into n itervals, and
#' return the center of each interval.
#' 
#' @param x A vector to bin
#' @param n Number of bins
#' @export
#' @family converting functions
#' @examples
#' rangebinc(seq(1:10), 3)
rangebinc <- function(x, n){
  x.bin <- seq(floor(min(x)), ceiling(max(x)), length.out=n+1)
  vbin(x.bin, n, type="c")[1:n]
}

#' Bined matrix of Random Field
#' 
#' Generate bined matrix given vectors of a random field.
#' 
#' @param x,y,v Random fields of (x, y) takeing value z. They must be
#'     vectors of the same length.
#' @param FUN A fun to calculate aggretated value in each matrix bin.
#' @param nx,ny Number of bins in each dimension.
#' @param na Replacemnet for NA value in matrix bins.
#' @export
#' @family converting functions
#' @examples
#' vbingrid(seq(1:20), seq(21:30), runif(20), na=0)
vbingrid <- function(x, y, v, FUN=mean, nx=50, ny=50, na=NA){
  if( length(x) != length(y) || length(x) != length(v))
    stop("Input x, y, v should be the same legnth.")
  
  x.int <- vbin(x, nx)
  y.int <- vbin(y, ny)
  
  df <- data.frame(x.int=x.int, y.int=y.int, z=v)
  dfg <- df %>%
    dplyr::group_by(x.int, y.int) %>%
    dplyr::summarise(z = FUN(z))
  
  mat <- matrix(data=na, nrow=nx, ncol=ny)
  mat[cbind(dfg$x.int, dfg$y.int)] <- dfg$z
  rownames(mat) <- rangebinc(x, nx)
  colnames(mat) <- rangebinc(y, ny)
  mat
}

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