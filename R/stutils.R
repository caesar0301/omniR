# convert UNIX hour (calculated by dividing UNIX seconds with 3600)
# to date at local time zone
hour2date <- function(hour, tz="Asia/Shanghai"){
  as.Date(as.POSIXct(hour*3600, origin="1970-01-01"), tz=tz)
}

# convert UNIX hour to Time-of-Day (TOD)
hour2tod <- function(hour, tz = 'Asia/Shanghai'){
  pt <- as.POSIXct(hour*3600, origin="1970-01-01")
  format(pt, "%H")
}

# convert UNIX hour to Time-of-Week (TOW)
hour2tow <- function(hour, tz='Asia/Shanghai'){
  pt <- as.POSIXct(hour*3600,origin="1970-01-01", tz=tz)
  weekdays(pt, abbreviate = TRUE)
}

# check if the given lon-lat pair falls into specific area
# area is a 4 length vector with lon-lat pairs of two points that
# define the area
in.area <- function(lon, lat, area){
  lon1 <- area[1]; lat1 <- area[2]
  lon2 <- area[3]; lat2 <- area[4]
  lons <- sort(c(lon1, lon2))
  lats <- sort(c(lat1, lat2))
  (lon >= lons[1] &
           lon <= lons[2] &
           lat >= lats[1] &
           lat <= lats[2])
}

# scale the value along two coordinates simultaneously, e.g.,
# along space and time in a random field models.
# alpha controls the weight of each coordinate in the scaled value.
stscale <- function(scoord, tcoord, value, alpha=0.5){
  df <- data.frame(s = scoord,
                   t = tcoord,
                   v = value)
  
  df2 <- df %>%
    # scaled over time
    group_by(s) %>%
    mutate(
      z.t = standardize(log10(1 + v))) %>%
    # scaled over space
    group_by(t) %>%
    mutate(
      z.s = standardize(log10(1 + v)),
      z = alpha * z.s + (1-alpha) * z.t) %>%
    select(-z.t, -z.s)
  
  df2$z
}

# calculate pair-wise distance metrics of row-wise locations
# @df - input data frame with (x, y) as spatial coordiantes
#       and z as an variable
# @return the column `r` represents distance; columns `z1`
# and `z2` represent the value pairs of calculated distance
dist.pw <- function(df){
  # calculate pair-wise distances
  # library(parallel)
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  blocks <- nrow(df)
  z.mean <- mean(df$z, na.rm=TRUE)
  z.var <- var(df$z, na.rm=TRUE)
  
  do.call(rbind, mclapply(seq(1, blocks), function(i){
    # print(sprintf("Pairs: %d/%d", i, blocks))
    do.call(rbind, mclapply(seq(i, blocks), function(j){
      data.frame(
        r = euc.dist(df[i, c("x","y")], df[j, c("x","y")]),
        z1 = df[i, "z"],
        z2 = df[j, "z"])
      }))
  }))
}