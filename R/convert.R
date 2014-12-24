# approximately matching x to y, and return the index of y
# a variate of match() or %in%
amatch <- function(x, y){
  sapply(x, function(i)which.min(abs(y-i)))
}

# bin a vector into n intervals in regard with its value range
# return the interval index if type is "i"
# return interval center if type is "c"
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

# bin the range of given vector into n itervals
# return the centers of intervals whose length equals n
rangebinc <- function(x, n){
  x.bin <- seq(floor(min(x)),
               ceiling(max(x)),
               length.out=n+1)
  vbin(x.bin, n, type="c")[1:n]
}

# Generate bined matrix given vectors of a random field
vbingrid <- function(x, y, value, FUN=mean, nx=50, ny=50, na=NA){
  x.int <- vbin(x, nx)
  y.int <- vbin(y, ny)
  
  df <- data.frame(x.int=x.int, y.int=y.int, z=value)
  dfg <- df %>%
    group_by(x.int, y.int) %>%
    summarise(z = FUN(z))
  
  mat <- matrix(data=na, nrow=nx, ncol=ny)
  mat[cbind(dfg$x.int, dfg$y.int)] <- dfg$z
  rownames(mat) <- rangebinc(x, nx)
  colnames(mat) <- rangebinc(y, ny)
  mat
}