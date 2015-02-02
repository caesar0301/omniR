# Calculate great circle distance. See
# http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
# Convert degrees to radians

#' @export
deg2rad <- function(deg) { deg * pi / 180 }
#' @export
rad2deg <- function(rad) { rad * 180 / pi }

#' Great circle distance calculation using the Spherical Law of Cosines
#' 
#' Calculates the geodesic distance between two points specified by radian
#' latitude/longitude using the Spherical Law of Cosines (slc).
#' 
#' @param long1,lat1 Longitude and latitude coordinates of point 1.
#' @param long2,lat2 Lontitude and latitude coordinates of point 2.
#' @family Great Circle Distance Calculation
#' @export
#' @examples
#' gcd.slc(120.0, 30.0, 120.5, 30.5)
gcd.slc <- function(long1, lat1, long2, lat2) {
  # Earth mean radius [km]
  R <- 6371
  t <- acos(sin(lat1)*sin(lat2)
            + cos(lat1)*cos(lat2) * cos(long2-long1))
  t * R
}

#' Great circle distance calculation using the Haversine formula
#' 
#' Calculates the geodesic distance between two points specified by radian
#' latitude/longitude using the Haversine formula (hf).
#' 
#' @param long1,lat1 Longitude and latitude coordinates of point 1.
#' @param long2,lat2 Lontitude and latitude coordinates of point 2.
#' @family Great Circle Distance Calculation
#' @export
#' @examples
#' gcd.hf(120.0, 30.0, 120.5, 30.5)
gcd.hf <- function(long1, lat1, long2, lat2) {
  # Earth mean radius [km]
  R <- 6371
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  R * c
}

#' Great circle distance calculation using Vincenty inverse formula
#' for ellipsoids
#' 
#' Calculates the geodesic distance between two points specified by radian
#' latitude/longitude using Vincenty inverse formula for ellipsoids (vif)
#' 
#' @param long1,lat1 Longitude and latitude coordinates of point 1.
#' @param long2,lat2 Lontitude and latitude coordinates of point 2.
#' @family Great Circle Distance Calculation
#' @export
#' @examples
#' gcd.vif(120.0, 30.0, 120.5, 30.5)
gcd.vif <- function(long1, lat1, long2, lat2) {
  # WGS-84 ellipsoid parameters:
  # length of major axis of the ellipsoid (radius at equator)
  a <- 6378137
  # ength of minor axis of the ellipsoid (radius at the poles)
  b <- 6356752.314245
  # flattening of the ellipsoid
  f <- 1/298.257223563

  # difference in longitude
  L <- long2-long1
  # reduced latitude
  U1 <- atan((1-f) * tan(lat1))
  # reduced latitude
  U2 <- atan((1-f) * tan(lat2))
  
  sinU1 <- sin(U1)
  cosU1 <- cos(U1)
  sinU2 <- sin(U2)
  cosU2 <- cos(U2)

  cosSqAlpha <- NULL
  sinSigma <- NULL
  cosSigma <- NULL
  cos2SigmaM <- NULL
  sigma <- NULL

  lambda <- L
  lambdaP <- 0
  iterLimit <- 100
  while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
    sinLambda <- sin(lambda)
    cosLambda <- cos(lambda)
    sinSigma <- sqrt(
      (cosU2*sinLambda) * (cosU2*sinLambda) +
        (cosU1*sinU2-sinU1*cosU2*cosLambda) * 
        (cosU1*sinU2-sinU1*cosU2*cosLambda) )

    if (sinSigma==0)
      return(0)  # Co-incident points
    
    cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
    sigma <- atan2(sinSigma, cosSigma)
    sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
    cosSqAlpha <- 1 - sinAlpha*sinAlpha
    cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
    
    if (is.na(cos2SigmaM))
      cos2SigmaM <- 0  # Equatorial line: cosSqAlpha=0
    
    C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
    lambdaP <- lambda
    lambda <- L + (1-C) * f * sinAlpha *
      (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
    iterLimit <- iterLimit - 1
  }
  
  if (iterLimit==0)
    return(NA)  # formula failed to converge
  
  uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
  A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
  B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
  deltaSigma = B*sinSigma*(
    cos2SigmaM+
      B/4*(cosSigma*(-1+2*cos2SigmaM^2)-
             B/6*cos2SigmaM*(-3+4*sinSigma^2)*(-3+4*cos2SigmaM^2)))
  s <- b*A*(sigma-deltaSigma) / 1000
  s
}