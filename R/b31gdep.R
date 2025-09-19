#' @title
#'  ASME B31G. Design pressure of pipe
#'
#' @family ASME B31G
#'
#' @description
#'  Calculate the design pressure that according to
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{
#'   ASME B31G-1991} is the conditioned construction characteristic that should
#'  not in no way exceeded.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param smys
#'  specified minimum yield of stress (\emph{SMYS}) as a characteristics of
#'  steel strength, [\emph{PSI}]. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param def
#'   appropriate (combined) design factor from
#'   \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.4.2002.pdf}{
#'    ASME B31.4},
#'   \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.8.2003.pdf}{
#'    ASME B31.8},
# nolint start: line_length_linter.
#'   or \href{https://www.asme.org/codes-standards/find-codes-standards/b31-11-slurry-transportation-piping-systems}{
# nolint end
#'    ASME B31.11}, []. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  Design pressure of pipe, [\emph{PSI}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{
#'   ASME B31G-1991}. Manual for determining the remaining strength of corroded
#'  pipelines. A supplement to \emph{ASTME B31} code for pressure piping.
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' b31gdep(30, .438, 52e3, .72)
#'
b31gdep <- function(d, wth, smys, def) {
  checkmate::assert_double(
    d,
    lower = 3.93e-2, upper = 1.27e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    wth,
    lower = 1.15e-2, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    smys,
    lower = 1e3, upper = 3e5, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    def,
    lower = 0, upper = 1, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(wth), length(smys), length(def)
  )))
  checkmate::assert_true(all(d - 2 * wth > 0.02))  # in inches

  2.0 * smys * wth * def / d + .5
}
