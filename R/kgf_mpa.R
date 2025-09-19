#' @title
#'  Megapascals to kilogram-force per square
#'
#' @family Measurement Unit Converter
#'
#' @description
#'  Convert pressure (stress) measured in
#'  \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{megapascals} (MPa)
#'  to
#'  \href{https://en.wikipedia.org/wiki/Kilogram-force_per_square_centimetre}{
#'  kilogram-force per square cm} (\emph{kgf/cm²}).
#'
#' @param x
#'  pressure (stress) measured in \emph{megapascals}, [\emph{MPa}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  Pressure (stress) in
#'  \emph{kilogram-force per square cm}, [\emph{kgf/cm²}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @seealso
#'  \code{\link{mpa_kgf}} for converting \emph{kilogram-force per square cm} to
#'  \emph{megapascals}
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' kgf_mpa(c(0.0980665, 1))
#'
kgf_mpa <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE, min.len = 1L)
  10.197162 * x
}
