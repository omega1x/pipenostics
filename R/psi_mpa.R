#' @title
#'  Convert megapascals to pounds per square inch
#'
#' @family utils
#'
#' @description
#'  Convert pressure (stress) measured in \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{megapascals} (MPa)
#'  to \href{https://en.wikipedia.org/wiki/Pounds_per_square_inch}{pounds per square inch} (PSI)
#'
#' @param x
#'  numeric vector of pressure (stress) measured in \emph{megapascals} (\emph{MPa})
#'
#' @return
#'  numeric vector of pressure (stress) measured in \emph{pounds per square inch} (\emph{PSI})
#'
#' @seealso
#'  \code{\link{mpa_psi}} for converting \emph{pounds per square inch} to \emph{megapascals}
#'
#' @export
#'
#' @examples
#'  psi_mpa(c(6.89475728e-3, 1))
#'  # [1] 1.0000 145.0377 # [PSI]
#'
#'  ## unit test:
#'  stopifnot(round(psi_mpa(c(6.89475728e-3, 1)), 4) == c(1, 145.0377))
#'
psi_mpa <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE)
  145.03773800721814*x
}
