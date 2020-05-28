#' @title
#'  ASME B31G. Allowable corrosion length in pipe
#'
#' @family ASME B31G functions
#'
#' @description
#'  Calculate allowable length of the corroded area in the pipe.
#'
#' @param dep
#'   design pressure of the pipe, [\emph{PSI}], numeric vector
#'
#' @param maop
#'  maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}], numeric
#'  vector
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
#'  allowable depth of the corroded area in the pipe, [\emph{inch}], numeric
#'  vector
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
#'  ## unit test:
#'  data(b31gdata)
#'  with(b31gdata,
#'       stopifnot(all.equal(
#'         b31gacl(design_pressure, maop, d, wth, depth, l),
#'         allowed_corrosion_length)))
#'
b31gacl <- function(dep, maop, d, wth, depth, l){
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
