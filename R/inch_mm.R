#' @title
#'  Millimeters to inches
#'
#' @family utils
#'
#' @description
#'  Convert length measured in \href{https://en.wikipedia.org/wiki/Millimetre}{millimeters} (mm)
#'  to \href{https://en.wikipedia.org/wiki/Inch}{inches}
#'
#' @param x
#'  numeric vector of lengths measured in \emph{millimeters}, [\emph{mm}].
#'  Type: \code{[double]}.
#'
#' @return
#'  numeric vector of lengths measured in \emph{inches}, [\emph{inch}]. Type: \code{[double]}.
#'
#' @seealso
#'  \code{\link{mm_inch}} for converting \emph{inches} to \emph{mm}
#'
#' @export
#'
#' @examples
#'  inch_mm(c(25.4, 1))
#'  # [1] 1.00000000 0.03937008  # [inch]
#'
#'  ## unit test:
#'  stopifnot(round(inch_mm(c(25.4, 1)), 8) == c(1.0, 0.03937008))
#'
inch_mm <- function(x) {
  checkmate::assert_double(x, finite = TRUE, any.missing = FALSE, min.len = 1)
  x/25.4
}
