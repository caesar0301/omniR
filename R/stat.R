# calculate the root mean squared error (RMSE)
RMSE <- function(x, y){
  if( !(class(x) == class(y)
        & class(x) %in% c("matrix", "numeric")) ) {
    stop("The input should be matrix or vector")
  }
  
  x = c(x); y = c(y)
  sqrt(mean((x-y)^2))
}

# normalize given vector
standardize <- function(x){
  x.min = min(x);
  x.max = max(x)
  (x - x.min) / (x.max - x.min)
}