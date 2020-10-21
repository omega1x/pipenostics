#' @title
#'  Convert pounds per square inch to megapascals
#'
#' @family utils
#'
#' @description
#'  Convert pressure (stress) measured in \href{https://en.wikipedia.org/wiki/Pounds_per_square_inch}{pounds per square inch} (PSI)
#'  to \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{megapascals} (MPa)
#'
#' @param x
#'  numeric vector of pressure (stress) measured in \emph{pounds per square inch} (\emph{PSI})
#'
#' @return
#'  numeric vector of pressure (stress) measured in \emph{megapascals} (\emph{MPa})
#'
#' @seealso
#'  \code{\link{psi_mpa}} for converting \emph{megapascals} to \emph{pounds per square inch}
#'
#' @export
#'
#' @examples
#'  mpa_psi(c(145.03773800721814, 1))
#'  # [1] 1.000000000 0.006894757 # [MPa]
#'
#'  ## unit test:
#'  stopifnot(round(mpa_psi(c(145.03773800721814, 1)), 8) == c(1, 6.89476e-3))
#'
mpa_psi <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE)
  6.89475728e-3*x
}