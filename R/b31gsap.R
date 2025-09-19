#' @title
#'  ASME B31G. Safe maximum pressure for the corroded area of pipe
#'
#' @family ASME B31G
#'
#' @description
#'  Calculate safe maximum pressure for the corroded area of pipe.
#'
#' @param dep
#'  design pressure of pipe, [\emph{PSI}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param depth
#'  measured maximum depth of the corroded area, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of the corroded area, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  Safe maximum pressure for the corroded area of pipe, [PSI].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{
#'    ASME B31G-1991}.Manual for determining the remaining strength of corroded
#'   pipelines. A supplement to \emph{ASTME B31} code for pressure piping.
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' # safe pressure is equal to design pressure:
#' b31gsap(1093, 30, .438, .1, 7.5)
#'
#' # safe pressure is lower than design pressure due corrosion:
#' b31gsap(877, 24, .281, .08, 15)
#'
b31gsap <- function(dep, d, wth, depth, l) {
  checkmate::assert_double(
    dep, lower = 0, upper = 6e3, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    d, lower = 3.93e-2, upper = 1.27e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    wth, lower = 1.15e-2, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    depth, lower = 0, upper = 2.54e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    l, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(dep), length(d), length(wth), length(depth), length(l)
  )))
  checkmate::assert_true(all(d - 2 * wth > 0.02))  # in inches

  A <- b31gafr(d, wth, l)
  d2w <- depth / wth
  sap <- trunc(
    ifelse(
      A > 4,
      1.1 * dep * (1.0 - d2w) + .5,
      1.1 * dep * (1.0 - 0.6666667 * d2w) /
        (1.0 - 0.6666667 * d2w / sqrt(A^2 + 1.0)) + .5
    )
  )
  ifelse(sap > dep, dep, sap)
}
