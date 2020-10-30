#' @title
#'  Shell92. Failure pressure of the corroded pipe
#'
#' @family Shell92
#'
#' @description
#'  Calculate failure pressure of the corroded pipe according to \emph{Shell92}
#'  code.
#'
#'  This code should be applied only to
#'  \itemize{
#'    \item single cross section of the pipeline containing a longitudinally
#'          oriented, flat bottom surface defect of the corrosion/erosion type;
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
#'  nominal outside diameter of the pipe, [\emph{mm}], numeric vector
#'
#' @param wth
#'  nominal wall thickness of the pipe, [\emph{mm}], numeric vector
#'
#' @param uts
#'  ultimate tensile strength (\emph{UTS}) or
#'  specified minimum tensile strength (\emph{SMTS}) as a
#'  characteristic of steel strength, [\emph{MPa}], numeric vector
#'
#' @param depth
#'  measured maximum depth of the corroded area, [\emph{mm}], numeric vector
#'
#' @param l
#'  measured maximum longitudial length of corroded area, [\emph{mm}],
#'  numeric vector
#'
#' @return
#'  Estimated failure pressure of the corroded pipe, [\emph{MPa}], numeric
#'  vector
#'
#' @references
#'  \enumerate{
#'    \item  D. Ritchie, S. Last, \emph{Burst criteria of corroded pipelines -
#'           defect acceptance criteria}, in Proceedings of the EPRG/PRC
#'           10th Biennial Joint Technical Meeting on Line Pipe Research
#'           (Cambridge, UK, 1995)
#'    \item S. Timashev and A. Bushinskaya, \emph{Diagnostics and Reliability
#'          of Pipeline Systems}, Topics in Safety, Risk, Reliability and Quality 30,
#'          \strong{DOI 10.1007/978-3-319-25307-7}
#'  }
#'
#' @seealso
#'   Other fail pressure functions: \code{\link{b31gpf}}, \code{\link{b31gmodpf}},
#'   \code{\link{dnvpf}}, \code{\link{pcorrcpf}}
#'
#' @export
#'
#' @examples
#'
#'  ## unit test:
#'  with(data.frame(
#'    d     = c(812.8, 219.0),  # [mm]
#'    wth   = c( 19.1,  14.5),  # [mm]
#'    uts  = c(530.9, 455.1),  # [N/mm^2]
#'    l     = c(203.2, 200.0),  # [mm]
#'    depth = c( 13.4,   9.0)   # [mm]
#'    ),
#'      stopifnot(
#'         all(round(shell92pf(d, wth, uts, depth, l), 4) == c(11.0926, 25.2729))
#'      )
#'  )
#'
shell92pf <- function(d, wth, uts, depth, l){
  checkmate::assert_double(d, lower = 1, upper = 5e3, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(wth, lower = 0, upper = 5e2, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(uts, lower = 0, upper = 5e3, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(depth, lower = 0, upper = 1e3, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(l, lower = 0, upper = 5e3, finite = TRUE, any.missing = FALSE)

  Q <- sqrt(1 + .805*l^2/d/wth)
  Pf <- 2*wth*.9*uts*(1 - depth/wth)/d/(1 - depth/wth/Q)
  Pf[depth >= .85*wth] <- NA_real_
  Pf
}
