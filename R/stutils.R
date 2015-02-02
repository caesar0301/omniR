#' Converting UNIX hour to Data object
#' 
#' Convert UNIX hour (calculated by dividing UNIX seconds with 3600)
#' to date at local time zone.
#' 
#' @param hour Hours from UNIX epoch
#' @param tz The time zone string
#' @export
hour2date <- function(hour, tz="Asia/Shanghai"){
  as.Date(as.POSIXct(hour*3600, origin="1970-01-01"), tz=tz)
}

#' Converting UNIX hour to Time-of-Day
#' 
#' Convert UNIX hour (calculated by dividing UNIX seconds with 3600)
#' to to Time-of-Day (TOD) at local time zone.
#' 
#' @param hour Hours from UNIX epoch
#' @param tz The time zone string
#' @export
hour2tod <- function(hour, tz = 'Asia/Shanghai'){
  pt <- as.POSIXct(hour*3600, origin="1970-01-01")
  format(pt, "%H")
}

#' Converting UNIX hour to Time-of-Week
#' 
#' Convert UNIX hour (calculated by dividing UNIX seconds with 3600)
#' to to Time-of-Week (TOW) at local time zone.
#' 
#' @param hour Hours from UNIX epoch
#' @param tz The time zone string
#' @export
hour2tow <- function(hour, tz='Asia/Shanghai'){
  pt <- as.POSIXct(hour*3600,origin="1970-01-01", tz=tz)
  weekdays(pt, abbreviate = TRUE)
}

#' Geographic area checking
#' 
#' Check if the given lon-lat pair falls into specific area.
#' The area is a 4-length vector with lon-lat pairs of two points that
#' confine the area boundaries.
#' @param lon,lat The point to be checked.
#' @param area The area defined by two points c(lon1, lat1, lon2, lat2).
#' @export
#' @examples
#' in.area(120.1, 30.1, c(120.0,30.0,120.5,30.5))
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

#' Spatio-temporal scaling
#' 
#' Scale the value along two coordinates simultaneously, e.g.,
#' along space and time in a random field models. 
#' 
#' @param scoord A vector defining spatial dimension of random field.
#' @param tcoord A vector defining temporal dimension of random field.
#' @param value A vector giving values in coorsponding random field slot.
#' @param alpha A tuning parameter controling the weight of spatial or temporal
#'     coordinate in the scaled value.
#' @export
#' @examples
#' scoord <- rep(seq(6), 2)
#' tcoord <- rep(c(1,2), each=6)
#' value <- runif(6 * 2)
#' stscale(scoord, tcoord, value)
stscale <- function(scoord, tcoord, value, alpha=0.5){
  df <- data.frame(s = scoord,
                   t = tcoord,
                   v = value)
  
  df2 <- df %>%
    # scaled over time
    dplyr::group_by(s) %>%
    dplyr::mutate(
      z.t = standardize(log10(1 + v))) %>%
    # scaled over space
    dplyr::group_by(t) %>%
    dplyr::mutate(
      z.s = standardize(log10(1 + v)),
      z = alpha * z.s + (1-alpha) * z.t) %>%
    dplyr::select(-z.t, -z.s)
  
  df2$z
}

#' @export
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#' Pair-wise distance calculation
#' 
#' Calculate pair-wise distance metrics of row-wise locations.
#' 
#' @param df Input data frame with (x, y) as spatial coordiantes
#'     and z as an variable
#' @return The column \code{r} represents Euclidean distance;
#'     columns \code{z1} and \code{z2} preserve corresponding \code{z}
#'     values of two points.
#' @export
#' @examples
#' dist.pw(data.frame(x=seq(6), y=seq(6), z=runif(6)))
dist.pw <- function(df){
  blocks <- nrow(df)
  z.mean <- mean(df$z, na.rm=TRUE)
  z.var <- var(df$z, na.rm=TRUE)
  
  do.call(rbind, mclapply(seq(1, blocks), function(i){
    do.call(rbind, mclapply(seq(i, blocks), function(j){
      data.frame(
        r = euc.dist(df[i, c("x","y")], df[j, c("x","y")]),
        z1 = df[i, "z"],
        z2 = df[j, "z"])
      }))
  }))
}
