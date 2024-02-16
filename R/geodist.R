#' @title
#'  Calculate distance between geographical objects
#'
#' @family utils
#'
#' @description
#'   Calculate distance between objects on \emph{Earth} using their absolute
#'   positions described by
#'   \href{https://en.wikipedia.org/wiki/Geographic_coordinate_system}{
#'   geographical coordinate system}
#'   in
#'   \href{https://en.wikipedia.org/wiki/Decimal_degrees}{decimal degrees}
#'   units denoted as \emph{DD}. The
#'   \href{https://en.wikipedia.org/wiki/Haversine_formula}{haversine formula}
#'   is applied to calculate the distance. The
#'   \href{https://en.wikipedia.org/wiki/Haversine_formula}{haversine formula},
#'   which considers the spherical model of \emph{Earth}, is well-used in
#'   navigation, giving great-circle distances between two points on a sphere
#'   from their longitudes and latitudes.
#'
#' @details
#'   \emph{DD} express latitude and longitude of
#'   \href{https://en.wikipedia.org/wiki/Geographic_coordinate_system}{
#'   geographical coordinate system} as
#'   decimal fractions and are used in many geographic information systems such
#'   as \href{https://maps.google.com}{Google Maps}. It is strongly recommended
#'   to use \href{https://maps.google.com}{Google Maps} to get
#'   exact objects' position in \emph{DD}.
#'
#'   Since several variants of \emph{Earth} radius can be accepted,
#'   the user is welcome to provide its own value.
#'   \href{https://en.wikipedia.org/wiki/World_Geodetic_System#WGS84}{WGS-84}
#'   \href{https://en.wikipedia.org/wiki/Earth_radius}{mean radius of semi-axes},
#'   \eqn{R_1}, is the default value.
#'
#'   The resulting distance is expressed in
#'   \href{https://en.wikipedia.org/wiki/Metre}{metres} (\emph{m}).
#'
#' @param lat1
#'   latitude of the first geographical object, [\emph{DD}].
#'   Type: \code{\link{assert_double}}.
#'
#' @param lon1
#'   longitude of the first geographical object, [\emph{DD}].
#'   Type: \code{\link{assert_double}}.
#'
#' @param lat2
#'   latitude of the second geographical object, [\emph{DD}].
#'   Type: \code{\link{assert_double}}.
#'
#' @param lon2
#'   longitude of the second geographical object, [\emph{DD}].
#'   Type: \code{\link{assert_double}}.
#'
#' @param earth
#'   \emph{Earth} radius, [\emph{m}]. See \strong{Details}.
#'   Type: \code{\link{assert_numeric}}.
#'
#' @return
#'   Distance between two geographical objects, [\emph{m}].
#'   Type: \code{\link{assert_double}}.
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' # Consider the longest linear pipeline segment in Krasnoyarsk, [DD]:
#' pipe <- list(
#'   lat1 = 55.98320350, lon1 = 92.81257226,
#'   lat2 = 55.99302417, lon2 = 92.80691885
#' )
#'
#' # and some official Earth radii, [m]:
#' R <- c(
#'   nominal_zero_tide_equatorial = 6378100.0000,
#'   nominal_zero_tide_polar      = 6356800.0000,
#'   equatorial_radius            = 6378137.0000,
#'   semiminor_axis_b             = 6356752.3141,
#'   polar_radius_of_curvature    = 6399593.6259,
#'   mean_radius_R1               = 6371008.7714,
#'   same_surface_R2              = 6371007.1810,
#'   same_volume_R3               = 6371000.7900,
#'   WGS84_ellipsoid_axis_a       = 6378137.0000,
#'   WGS84_ellipsoid_axis_b       = 6356752.3142,
#'   WGS84_ellipsoid_curvature_c  = 6399593.6258,
#'   WGS84_ellipsoid_R1           = 6371008.7714,
#'   WGS84_ellipsoid_R2           = 6371007.1809,
#'   WGS84_ellipsoid_R3           = 6371000.7900,
#'   GRS80_axis_a                 = 6378137.0000,
#'   GRS80_axis_b                 = 6356752.3141,
#'   spherical_approx             = 6366707.0195,
#'   meridional_at_the_equator    = 6335439.0000,
#'   Chimborazo_maximum           = 6384400.0000,
#'   Arctic_Ocean_minimum         = 6352800.0000,
#'   Averaged_center_to_surface   = 6371230.0000
#' )
#'
#' # Calculate length of the pipeline segment for different radii:
#' len <- with(
#'   pipe, vapply(
#'     R, geodist, double(1), lat1 = lat1, lon1 = lon1, lat2 = lat2, lon2 = lon2
#'   )
#' )
#'
#' print(range(len))
#'
#' # [1] 1140.82331483 1152.37564656 #  [m]
#'
#'
#' # Consider some remarkable objects on Earth, [DD]:
#' objects <- rbind(
#'   Mount_Kailash      = c(lat = 31.069831297551982, lon =  81.31215667724196),
#'   Easter_Island_Moai = c(lat =-27.166873910247862, lon =-109.37092217323053),
#'   Great_Pyramid      = c(lat = 29.979229451772856, lon =  31.13418110843685),
#'   Antarctic_Pyramid  = c(lat = -79.97724194984573, lon = -81.96170583068950),
#'   Stonehendge        = c(lat = 51.179036665131870, lon =-1.8262150017463086)
#' )
#'
#' # Consider all combinations of distances between them:
#' path <- t(combn(rownames(objects), 2))
#'
#' d <- geodist(
#'   lat1 = objects[path[, 1], "lat"],
#'   lon1 = objects[path[, 1], "lon"],
#'   lat2 = objects[path[, 2], "lat"],
#'   lon2 = objects[path[, 2], "lon"]
#' )*1e-3
#'
#' cat(
#'   paste(
#'     sprintf("%s <-> %s: %1.4f km", path[, 1], path[, 2], d),
#'     collapse = "\n"
#' )
#' )

#' # Mount_Kailash <-> Easter_Island_Moai: 18890.9362 km
#' # Mount_Kailash <-> Great_Pyramid: 4765.7923 km
#' # Mount_Kailash <-> Antarctic_Pyramid: 14523.7267 km
#' # Mount_Kailash <-> Stonehendge: 6917.4240 km
#' # Easter_Island_Moai <-> Great_Pyramid: 16164.4674 km
#' # Easter_Island_Moai <-> Antarctic_Pyramid: 6010.1422 km
#' # Easter_Island_Moai <-> Stonehendge: 13520.3511 km
#' # Great_Pyramid <-> Antarctic_Pyramid: 13726.9374 km
#' # Great_Pyramid <-> Stonehendge: 3595.6153 km
#' # Antarctic_Pyramid <-> Stonehendge: 15396.3978 km

geodist <- function(lat1, lon1, lat2, lon2, earth = 6371008.7714){
  checkmate::assert_double(
    lat1, lower = -90, upper = 90, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lon1, lower = -180, upper = 180, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lat2, lower = -90, upper = 90, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    lon2, lower = -180, upper = 180, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_true(all.commensurable(c(
    length(lat1), length(lon1), length(lat2), length(lon2)
  )))
  checkmate::assert_number(earth, lower = 6335439.0000, upper = 6399593.6259)

  POW <- .Primitive("^")
  PI  <- base::pi

  lat1 = lat1*PI/180
  lat2 = lat2*PI/180

  cl1 = cos(lat1)
  cl2 = cos(lat2)
  sl1 = sin(lat1)
  sl2 = sin(lat2)
  delta = (lon2 - lon1)*PI/180
  cos_delta = cos(delta)

  y = sqrt(POW(cl2 * sin(delta), 2) + POW(cl1 * sl2 - sl1 * cl2 * cos_delta, 2))
  x = sl1 * sl2 + cl1 * cl2 * cos_delta
  atan2(y,x) * earth
}

