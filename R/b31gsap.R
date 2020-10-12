#' @title
#'  ASME B31G. Safe maximum pressure for the corroded area of pipe
#'
#' @family ASME B31G functions
#'
#' @description
#'  Calculate safe maximum pressure for the corroded area of pipe.
#'
#' @param dep
#'  design pressure of the pipe, [\emph{PSI}], numeric vector
#'
#' @param d
#'  nominal outside diameter of the pipe, [\emph{inch}], numeric vector
#'
#' @param wth
#'  nominal wall thickness of the pipe, [\emph{inch}], numeric vector
#'
#' @param depth
#'   measured maximum depth of the corroded area, [\emph{inch}], numeric vector
#'
#' @param l
#'  measured maximum longitudial length of the corroded area, [\emph{inch}],
#'  numeric vector
#'
#' @return
#'  Safe maximum pressure for the corroded area of pipe, [PSI], numeric vector
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}.
#'  Manual for determining the remaining strength of corroded pipelines. A
#'  supplement to \emph{ASTME B31} code for pressure piping.
#'
#' @export
#'
#' @examples
#'  b31gsap(1093, 30, .438, .1, 7.5)
#'  # [1] 1093  # [PSI], safe pressure is equal to design pressure
#'
#'  b31gsap(877, 24, .281, .08, 15)
#'  # [1] 690   # [PSI], safe pressure is lower than design pressure due corrosion
#'
#'  ## unit test:
#'  data(b31gdata)
#'  with(b31gdata,
#'       stopifnot(b31gsap(design_pressure, d, wth, depth, l) == safe_pressure))
#'
b31gsap <- function(dep, d, wth, depth, l){
  checkmate::assert_double(dep, lower = 0, upper = 6e3, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(d, lower = 3.93e-2, upper = 1.27e5, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(wth, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(depth, lower = 0, upper = 2.54e4, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(l, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE)

  A <- b31gafr(d, wth, l)
  d2w <- depth/wth
  sap <- trunc(
    ifelse(A > 4,
           1.1*dep*(1 - d2w) + .5,
           1.1*dep*(1 - 2/3*d2w)/(1 - 2/3*d2w/sqrt(A^2 + 1)) + .5)
  )
  ifelse(sap > dep, dep, sap)
}
