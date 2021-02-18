#' @title
#'  ASME B31G. Allowable corrosion length in pipe
#'
#' @family ASME B31G functions
#'
#' @description
#'  Calculate allowable length of the corroded area in the pipe.
#'
#' @param dep
#'   design pressure of the pipe, [\emph{PSI}]. Type: \code{\link{assert_double}}.
#'
#' @param maop
#'  maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param d
#'  nominal outside diameter of the pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of the pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
#'
#' @param depth
#'   measured maximum depth of the corroded area, [\emph{inch}].
#'   Type: \code{\link{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of the corroded area, [\emph{inch}].
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  allowable length of the corroded area in the pipe, [\emph{inch}].
#'  Type: \code{\link{assert_double}}.
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}.
#'  Manual for determining the remaining strength of corroded pipelines. A
#'  supplement to \emph{ASTME B31} code for pressure piping.
#'
#' @export
#'
#' @examples
#'  b31gacl(1093, 910, 30, .438, .1, 7.5)
#'  # [1] Inf  # [inch] - corrosion is low, no limit for the corroded area length
#'
#'  b31gacl(438, 400, 20, .25, .18, 10)
#'  # [1] 2.018  # [inch] - finite allowed length of the corroded area
#'
#'
b31gacl <- function(dep, maop, d, wth, depth, l){
  checkmate::assert_double(dep, lower = 0, upper = 6e3, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(maop, lower = 25.4, upper = 1.27e5, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(d, lower = 3.93e-2, upper = 1.27e5, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(wth, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(depth, lower = 0, upper = 2.54e4, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(l, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE, min.len = 1)

  PS <- trunc(1.1*dep*(1 - depth/wth) + .5)
  PS[PS > dep] <- dep[PS > dep]
  J <- 2/3*depth/wth
  AP <- rep(Inf, length(J))
  x  <- rep(0  , length(J))
  cnd <- maop > PS
  AP[cnd] <- 1e-3*trunc(
    1e3*sqrt((J[cnd]/(1 - 1.1*dep[cnd]*(1 - J[cnd])/maop[cnd]))^2 - 1) + .5)
  x[cnd] <- trunc(
    1.1*dep[cnd]*(1 - J[cnd])/(1 - 2/3*depth[cnd]/sqrt(
      wth[cnd]*AP[cnd]^2 + 1)) + .5)
  cnd2 <- cnd & (x > dep)
  x[cnd2] <- dep[cnd2]
  AP[cnd & (x > maop | AP > 4)] <- 4

  # Allowable corrosion length, [inch]
  trunc(1000*sqrt(d*wth)*1.12*AP)*.001
}
