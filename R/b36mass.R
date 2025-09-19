#' @title
#'  ASME B36.10M. Estimate pipe mass
#'
#' @family ASME B36.10M
#'
#' @description
#'  Estimate mass or geometric specifications of the manufactured pipes
#'
#' @details
#'   The mass of a one-meter pipe segment is determined by linear interpolation
#'   based on the tabular data provided in the specified origins (see
#'   \code{\link{b36pipedata}}). If the \code{origin} of the initial data is
#'   \code{NULL} (default), the mass of the pipe is calculated using a formula:
#'
#'   \deqn{M = 10^{-3} \pi \rho w \left(d - w \right ) \cdot l}
#'
#'   where
#'   \itemize{
#'      \item \eqn{M} - mass of a pipe, [\emph{kg}]
#'      \item \eqn{\rho} - mass density of pipe material, [\emph{g/cm³}]
#'      \item \eqn{w} - nominal wall thickness of the manufactured pipe,
#'                      [\emph{mm}]
#'      \item \eqn{d} - nominal outside diameter of the manufactured pipe,
#'                      [\emph{mm}]
#'      \item \eqn{l} - actual pipe length, [\emph{m}]
#'   }
#' For \code{origin} in \code{c(1, 2, 4, 5)} the values provided in the
#' \code{\link{b36pipedata}} match the calculated values obtained from the
#' formula with an accuracy of 1 \%.
#'
#' Inverse calculations \code{\link{b36wth}} and \code{\link{b36d}} are
#' performed using algebraically derived formulas.
#'
#' @param d
#'   nominal outside diameter of pipe, [\emph{mm}].
#'   Type: \code{\link[checkmate]{assert_double}}
#'
#' @param wth
#'   nominal wall thickness of pipe, [\emph{mm}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param len
#'   pipe length, [\emph{m}]. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param rho
#'   mass density of pipe material, [\emph{g/cm³}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param origin
#'   identifier for the information origin regarding the specifications of pipe.
#'   Type: \code{\link[checkmate]{assert_integer}}.
#'
#' @param mass
#'   actual mass of pipe, [\emph{kg}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  \describe{
#'   \item{for \code{\link{b36mass}}}{pipe mass, [\emph{kg}]}
#'   \item{for \code{\link{b36wth}}}{pipe wall thickness, [\emph{mm}]}
#'   \item{for \code{\link{b36d}}}{outside diameter of pipe, [\emph{mm}]}
#'  }
#'
#' @examples
#' library(pipenostics)
#'
#' # Since some specification origins provide the mass of a one-meter pipe
#' # segment taking into account possible deviations during its production
#' # process, when the user specifies the origin ID directly, the mass values
#' # are calculated using linear interpolation:
#' b36mass(68, 13, rho = 7.9, origin = 7L)
#'
#' # The discrepancy with the calculations based on the formula can be more
#' # than 7 %:
#' b36mass(68, 13, rho = 7.9, origin = NULL)
#'
#' # For origins which are ASME B36 standards such differences should be
#' # minimal:
#' b36mass(965, 10.31, origin = 1L) - b36mass(965, 10.31, origin = NULL)
#'
#' # The calculations of diameter and wall thickness are straightforward and
#' # use only inverse formulas without origin references:
#' b36d(10.31, 242.74)
#'
#' b36wth(965, 242.74)
#'
#' @rdname b36mass
#' @export
b36mass <- function(d, wth, len = 1, rho = 7.85, origin = NULL) {
  L_ORIGIN    <- "origin"
  L_DIAMETER  <- "d"
  L_THICKNESS <- "wth"
  L_LENGTH    <- "len"
  L_DENSITY   <- "rho"
  L_MASS      <- "mass"
  L_ONE       <- L_FIRST <- 1L
  L_TWO       <- 2
  D_OFFSET    <-  .4  # Pipe diameter tolerance, [mm]

  md <- pipenostics::b36pipedata
  checkmate::assert_double(
    d,
    lower = min(md[[L_DIAMETER]]) - D_OFFSET,
    upper = max(md[[L_DIAMETER]]) + D_OFFSET,
    any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    wth,
    lower = min(md[[L_THICKNESS]]), upper = max(md[[L_THICKNESS]]),
    any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    len,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    rho,
    lower = 7.75, upper =  8.05, any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(wth), length(len), length(rho)
  )))
  checkmate::assert_true(all(d - 2 * wth > 0.5))  # in mm
  checkmate::assert_double(
    wth / d,
    lower = .Machine[["double.eps"]],
    upper = max(md[[L_THICKNESS]] / md[[L_DIAMETER]])
  )
  checkmate::assert_integer(
    origin,
    lower = min(md[[L_ORIGIN]]), upper = max(md[[L_ORIGIN]]),
    any.missing = FALSE, min.len = L_ONE,
    max.len = length(unique(md[[L_ORIGIN]])), unique = TRUE, null.ok = TRUE
  )

  pipe_mass <- (d - wth) * 1e-3 * wth * rho * len * base::pi
  if (is.null(origin)) return(pipe_mass)

  C <- data.frame(
    a = c(
      1.34639486196096e-05, -0.269421768471856, -0.543828305826526,
      -0.32040873191324,    -0.001023302221079,  0.000750308148511,
      -8.88387684950809e-05
    ),
    b = c(
      0.99993868472687, 1.06490339805795, 1.07025471029979,
      1.04765386422189, 1.00035194161928, 0.999990376939387,
      1.00000056550023
    )
  )
  pipe <- data.frame(d = d, wth = wth, len = len, rho = rho)
  md   <- md[md[[L_ORIGIN]] %in% origin, ]
  vapply(
    seq_len(nrow(pipe)),
    function(i) {
      m <- md[
        md[[L_DIAMETER]]  == pipe[i, L_DIAMETER]  &
        md[[L_THICKNESS]] == pipe[i, L_THICKNESS] &
        md[[L_DENSITY]]   == pipe[i, L_DENSITY],
        L_MASS
      ]
      if (isTRUE(as.logical(length(m)))) return(m[[L_FIRST]])
      if (
        pipe[i, L_DIAMETER] < min(md[[L_DIAMETER]]) - D_OFFSET |
        pipe[i, L_DIAMETER] > max(md[[L_DIAMETER]]) + D_OFFSET
      ) return(NA_real_)
      dst <- {
        (md[[L_DIAMETER]]  - pipe[i, L_DIAMETER])^L_TWO +
        (md[[L_THICKNESS]] - pipe[i, L_THICKNESS])^L_TWO +
        (md[[L_DENSITY]]   - pipe[i, L_DENSITY])^L_TWO
      }
      oid <- md[which.min(dst), L_ORIGIN]
      pipe_mass[[i]] * C[oid, "b"] + C[oid, "a"]
    },
    double(L_ONE)
  ) * pipe[[L_LENGTH]]
}

#' @rdname b36mass
#' @export
b36d <- function(wth, mass, len = 1, rho = 7.85) {
  L_THICKNESS <- "wth"
  L_MASS      <- "mass"
  L_ONE       <- 1L

  md <- pipenostics::b36pipedata
  checkmate::assert_double(
    wth,
    lower = min(md[[L_THICKNESS]]), upper = max(md[[L_THICKNESS]]),
    any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    mass,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    len,
    lower = 0.1, finite = TRUE, any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    rho,
    lower = 7.75, upper =  8.05, any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_true(commensurable(c(
    length(wth), length(mass), length(len), length(rho)
  )))

  unit_mass <- mass / len
  checkmate::assert_double(
    unit_mass,
    lower = min(md[[L_MASS]]), upper = max(md[[L_MASS]]),
    any.missing = FALSE, min.len = L_ONE
  )
  1e3 * unit_mass / base::pi / wth / rho + wth
}

#' @rdname b36mass
#' @export
b36wth <- function(d, mass, len = 1, rho = 7.85) {
  L_DIAMETER  <- "d"
  L_MASS      <- "mass"
  L_ONE       <- 1L
  D_OFFSET    <-  .4  # Pipe diameter tolerance, [mm]

  md <- pipenostics::b36pipedata
  checkmate::assert_double(
    d,
    lower = min(md[[L_DIAMETER]]) - D_OFFSET,
    upper = max(md[[L_DIAMETER]]) + D_OFFSET,
    any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    mass,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    len,
    lower = 0.1, finite = TRUE, any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_double(
    rho,
    lower = 7.75, upper =  8.05, any.missing = FALSE, min.len = L_ONE
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(mass), length(len), length(rho)
  )))

  unit_mass <- mass / len
  checkmate::assert_double(
    unit_mass,
    lower = min(md[[L_MASS]]), upper = max(md[[L_MASS]]),
    any.missing = FALSE, min.len = L_ONE
  )

  r <- .5 * d
  D <- r^2 - unit_mass / base::pi / rho * 1e3
  D[D < 0] <- NA_real_
  r - sqrt(D)
}
