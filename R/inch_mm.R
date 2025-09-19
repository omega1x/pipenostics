#' @title
#'  Millimeters to inches
#'
#' @family Measurement Unit Converter
#'
#' @description
#'  Convert length measured in
#'  \href{https://en.wikipedia.org/wiki/Millimetre}{millimeters} (mm)
#'  to \href{https://en.wikipedia.org/wiki/Inch}{inches}
#'
#' @param x
#'  length measured in \emph{millimeters}, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  Length in \emph{inches}, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @seealso
#'  \code{\link{mm_inch}} for converting \emph{inches} to \emph{mm}
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' inch_mm(c(25.4, 1))
#'
inch_mm <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE, min.len = 1L)
  x / 25.4
}
