#' @title
#'  Megapascals to kilogram-force per square
#'
#' @family utils
#'
#' @description
#'  Convert pressure (stress) measured in \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{megapascals} (MPa)
#'  to \href{https://en.wikipedia.org/wiki/Kilogram-force_per_square_centimetre}{kilogram-force per square cm} (\eqn{kgf/cm^2})
#'
#' @param x
#'  numeric vector of pressure (stress) measured in \emph{megapascals} (\emph{MPa})
#'
#' @return
#'  numeric vector of pressure (stress) measured in \emph{kilogram-force per square cm} (\emph{kgf/cm^2})
#'
#' @seealso
#'  \code{\link{mpa_kgf}} for converting \emph{kilogram-force per square cm} to \emph{megapascals}
#'
#' @export
#'
#' @examples
#'
#'  ## unit test:
#'  stopifnot(
#'    all.equal(
#'      kgf_mpa(c(0.0980665, 1)),
#'      c(1.0000000, 10.197162)
#'    )
#'  )
#'
kgf_mpa <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE)
  10.197162*x
}
