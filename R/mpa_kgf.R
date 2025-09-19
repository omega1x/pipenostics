#' @title
#'   Kilogram-force per square cm to megapascals
#'
#' @family Measurement Unit Converter
#'
#' @description
#'   Convert pressure (stress) measured in
#'   \href{https://en.wikipedia.org/wiki/Kilogram-force_per_square_centimetre}{
#'    kilogram-force per square cm} (\emph{kgf/cm²})
#'   to \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{megapascals} (MPa)
#'
#' @param x
#'   pressure (stress) measured in \emph{kilogram-force per square cm},
#'   [\emph{kgf/cm²}]. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'   Pressure (stress) in \emph{megapascals}, [\emph{MPa}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @seealso
#'   \code{\link{kgf_mpa}} for converting \emph{megapascals} to
#'   \emph{kilogram-force per square cm}
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' mpa_kgf(c(10.1971619998, 1))
#'
mpa_kgf <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE, min.len = 1L)
  9.806650125e-2 * x
}
