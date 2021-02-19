#' @title
#'  Megapascals to kilogram-force per square
#'
#' @family utils
#'
#' @description
#'  Convert pressure (stress) measured in \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{megapascals} (MPa)
#'  to \href{https://en.wikipedia.org/wiki/Kilogram-force_per_square_centimetre}{kilogram-force per square cm} (\eqn{kgf/cm^2}).
#'
#' @param x
#'  pressure (stress) measured in \emph{megapascals},
#'  [\emph{MPa}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  pressure (stress) in
#'  \emph{kilogram-force per square cm}, [\emph{kgf/cm^2}].
#'  Type: \code{\link{assert_double}}.
#'
#' @seealso
#'  \code{\link{mpa_kgf}} for converting \emph{kilogram-force per square cm} to \emph{megapascals}
#'
#' @export
#'
#' @examples
#'  kgf_mpa(c(0.0980665, 1))
#'  # [1]  1.00000 10.19716
#'
#'
kgf_mpa <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE, min.len = 1)
  10.197162*x
}
