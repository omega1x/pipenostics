#' @title
#'  ASME B31G. Operational status of pipe
#'
#' @family ASME B31G functions
#'
#' @description
#'  Determine the operational status of pipe: is it excellent? or is
#'  technological control required? or is it critical situation?
#'
#' @param wth
#'  nominal wall thickness of the pipe, [\emph{inch}], numeric vector
#'
#' @param depth
#'  measured maximum depth of the corroded area, [\emph{inch}], numeric vector
#'
#' @return
#'  Operational status of pipe, numeric vector of 3 possible values:
#'  \itemize{
#'    \item \emph{1} - excellent
#'    \item \emph{2} - monitoring is recommended
#'    \item \emph{3} - alert! replace the pipe immediately!
#'  }
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}.
#'  Manual for determining the remaining strength of corroded pipelines. A
#'  supplement to \emph{ASTME B31} code for pressure piping.
#'
#' @export
#'
#' @examples
#'  b31gops(.438, .1)
#'  # [1] 2  # typical status for the most of pipes
#'
#'  b31gops(.5, .41)
#'  # [1] 3  # alert! Corrosion depth is too high! Replace the pipe!
#'
#'  ## unit test:
#'  data(b31gdata)
#'  with(b31gdata, stopifnot(all(b31gops(wth, depth) & status)))
#'
b31gops <- function(wth, depth){
  checkmate::assert_double(wth, lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(depth, lower = 0, upper = 2.54e4, finite = TRUE, any.missing = FALSE)

  a <- .8*wth  # alert setting
  1 + (depth > .1*wth & depth <= a) + 2*(depth > a)
}
