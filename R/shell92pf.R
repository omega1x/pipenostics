#' @title
#'  Shell92. Failure pressure of the corroded pipe
#'
#' @family Failure estimation
#'
#' @description
#'  Calculate failure pressure of the corroded pipe according to \emph{Shell92}
#'  code.
#'
#'  This code should be applied only to
#'  \itemize{
#'    \item single cross section of the pipeline containing a longitudinally
#'          oriented, flat bottom surface defect of corrosion/erosion type;
#'    \item defects which depth is less than 85 \% of pipe wall thickness.
#'  }
#'
#'  The estimation is valid for single isolated metal loss defects of
#'  the corrosion/erosion type and when only internal pressure loading
#'  is considered.
#'
#'  As in the case of \code{\link{dnvpf}}, the defect is approximated by a
#'  rectangular form.
#'
#' @details
#'   Numeric \code{NA}s may appear in case prescribed conditions of
#'   use are offended.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param uts
#'  ultimate tensile strength (\emph{UTS}) or
#'  specified minimum tensile strength (\emph{SMTS}) as a
#'  characteristic of steel strength, [\emph{MPa}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param depth
#'  measured maximum depth of the corroded area, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of corroded area, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  Estimated failure pressure of the corroded pipe, [\emph{MPa}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @references
#'  Timashev and A. Bushinskaya, \emph{Diagnostics and Reliability
#'  of Pipeline Systems}, Topics in Safety, Risk, Reliability and Quality 30,
#'  \doi{10.1007/978-3-319-25307-7}.
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' # Consider two pipes with the next specifications:
#' d     = c(812.8, 219.0)  # [mm]
#' wth   = c( 19.1,  14.5)  # [mm]
#' uts  = c(530.9, 455.1)   # [N/mm²]
#' l     = c(203.2, 200.0)  # [mm]
#' depth = c( 13.4,   9.0)  # [mm]
#'
#' # Get the failure pressure for that pipes:
#' shell92pf(d, wth, uts, depth, l)
#'
shell92pf <- function(d, wth, uts, depth, l) {
  checkmate::assert_double(
    d,
    lower = 1, upper = 5e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    wth,
    lower = 0.29, upper = 5e2, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    uts,
    lower = 5, upper = 2e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    depth,
    lower = 0, upper = 1e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    l,
    lower = 0, upper = 5e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(wth), length(uts), length(depth), length(l)
  )))
  checkmate::assert_true(all(d - 2 * wth > 0.5))  # in mm

  Q <- sqrt(1 + .805 * l^2 / d / wth)
  Pf <- 2 * wth * .9 * uts * (1 - depth / wth) / d / (1 - depth / wth / Q)
  Pf[depth >= .85 * wth] <- NA_real_
  Pf
}
