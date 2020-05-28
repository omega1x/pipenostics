#' @title
#'  ASME B31G. A-factor
#'
#' @family ASME B31G functions
#'
#' @description
#'  Calculate intermediate factor related to the geometry of the corroded zone.
#'
#' @param d
#'  nominal outside diameter of the pipe, [\emph{inch}], numeric vector
#'
#' @param wth
#'  nominal wall thickness of the pipe, [\emph{inch}], numeric vector
#'
#' @param l
#'  measured maximum longitudial length of the corroded area, [\emph{inch}],
#'  numeric vector
#'
#' @return
#'  Intermediate factor related to the geometry of the corroded area, [],
#'  numeric vector
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}.
#'  Manual for determining the remaining strength of corroded pipelines. A
#'  supplement to \emph{ASTME B31} code for pressure piping.
#'
#' @export
#'
#' @examples
#'  b31gafr(30, .438, 7.5)
#'  # [1] 1.847  # A-factor is less than 5, so the corrosion is not critical
#'
#'  ## unit test:
#'  data(b31gdata)
#'  with(b31gdata, all.equal(b31gafr(d, wth, l), A))
#'
b31gafr <- function(d, wth, l) 1e-3*trunc(1e3*.893*l/sqrt(d*wth))
