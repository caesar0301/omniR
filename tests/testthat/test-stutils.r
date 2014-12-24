context("stutils")

test_that( "a fast generation of location distance matrix", {
  x <- c(23, 14, 23)
  y <- c(91,10,30)
  z <- c(1, 2, 3)
  df <- data.frame(x=x, y=y, z=z)
  dist.pw(df)
})