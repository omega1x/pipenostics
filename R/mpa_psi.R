#' @title
#'  Pounds per square inch to megapascals
#'
#' @family Measurement Unit Converter
#'
#' @description
#'  Convert pressure (stress) measured in
#'  \href{https://en.wikipedia.org/wiki/Pounds_per_square_inch}{pounds per
#'   square inch} (PSI) to \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{
#'   megapascals} (MPa)
#'
#' @param x
#'   pressure (stress) measured in \emph{pounds per square inch} (\emph{PSI}).
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'   Pressure (stress) in \emph{megapascals} (\emph{MPa}).
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @seealso
#'   \code{\link{psi_mpa}} for converting \emph{megapascals} to
#'   \emph{pounds per square inch}
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' mpa_psi(c(145.03773800721814, 1))
#'
mpa_psi <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE)
  6.89475728e-3 * x
}
