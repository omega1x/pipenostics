#' @title
#'  DNV-RP-F101. Failure pressure of the corroded pipe
#'
#' @family DNV-RP-F101 functions
#'
#' @description
#'  Calculate failure pressure of the corroded pipe
#'  according to \emph{Section 8.2} of
#'  in \href{https://rules.dnvgl.com/docs/pdf/DNV/codes/docs/2010-10/RP-F101.pdf}{DNV-RP-F101}.
#'  The estimation is valid for single isolated metal loss defects of
#'  the corrosion/erosion type and when only internal pressure loading
#'  is considered.
#'
#' @details
#'   In contrast to
#'   \href{https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines}{ASME B31G-2012}
#'   property of pipe metal is characterized by specified minimum tensile
#'   strength - \emph{SMTS}, [\eqn{N/mm^2}], and \href{https://en.wikipedia.org/wiki/International_System_of_Units}{SI}
#'   is default unit system. \emph{SMTS} is given in the linepipe steel
#'   material specifications (e.g. \href{https://www.api.org/products-and-services/standards/important-standards-announcements/standard-5l}{API 5L})
#'   for each material grade.
#'
#'   At the same time \emph{Timashev et al.} used ultimate tensile strength
#'   - \href{https://en.wikipedia.org/wiki/Ultimate_tensile_strength}{UTS}
#'   in place of \emph{SMTS}. So, for the case those quantities may be used in
#'   interchangeable way.
#'
#'   Numeric \code{NA}s may appear in case prescribed conditions of
#'   use are offended.
#'
#' @param d
#'  nominal outside diameter of the pipe, [\emph{mm}], numeric vector
#'
#' @param wth
#'  nominal wall thickness of the pipe, [\emph{mm}], numeric vector
#'
#' @param smts
#'  specified minimum tensile strength (\emph{SMTS}) or
#'  ultimate tensile strength (\emph{UTS}) as a
#'  characteristic of steel strength, [\eqn{N/mm^2}], numeric vector
#'
#' @param depth
#'  measured maximum depth of the corroded area, [\emph{mm}], numeric vector
#'
#' @param l
#'  measured maximum longitudial length of corroded area, [\emph{mm}],
#'  numeric vector
#'
#' @return
#'  Estimated failure pressure of the corroded pipe, [\eqn{N/mm^2}], numeric
#'  vector
#'
#' @references
#'  \enumerate{
#'  \item Recommended practice \href{https://rules.dnvgl.com/docs/pdf/DNV/codes/docs/2010-10/RP-F101.pdf}{DNV-RP-F101}.
#'    Corroded pipelines. \strong{DET NORSKE VERITAS}, October 2010.
#'  \item \href{https://www.techstreet.com/standards/asme-b31g-2012-r2017?product_id=1842873}{ASME B31G-2012}.
#'    Manual for determining the remaining strength of corroded pipelines:
#'    supplement to \emph{B31 Code} for pressure piping.
#'  \item  S. Timashev and A. Bushinskaya, \emph{Diagnostics and Reliability
#'    of Pipeline Systems}, Topics in Safety, Risk, Reliability and Quality 30,
#'    \strong{DOI 10.1007/978-3-319-25307-7}
#'  }
#'
#' @seealso
#'   Other fail pressure functions: \code{\link{b31gpf}}, \code{\link{b31gmodpf}}
#'
#' @export
#'
#' @examples
#'
#'  ## unit test:
#'  with(data.frame(
#'    d     = c(812.8, 219.0),  # [mm]
#'    wth   = c( 19.1,  14.5),  # [mm]
#'    smts  = c(530.9, 455.1),  # [N/mm^2]
#'    l     = c(203.2, 200.0),  # [mm]
#'    depth = c( 13.4,   9.0)   # [mm]
#'  ),
#'    stopifnot(all(round(
#'      dnvpf(d, wth, smts, depth, l), 4) == c(15.8663, 34.0118))))  # [N/mm^2]
#'
dnvpf <- function(d, wth, smts, depth, l){
  checkmate::assert_double(d, lower = 1, upper = 5e3, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(wth, lower = 0, upper = 5e2, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(smts, lower = 0, upper = 5e3, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(depth, lower = 0, upper = 1e3, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(l, lower = 0, upper = 5e3, finite = TRUE, any.missing = FALSE)

  Q <- sqrt(1 + .31*l^2/d/wth)
  Pf <- 2*wth*smts*(1 - depth/wth)/(d - wth)/(1 - depth/wth/Q)
  Pf[depth >= .85*wth] <- NA_real_
  Pf
}
