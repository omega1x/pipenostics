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
#'  numeric vector of lengths measured in \emph{millimeters} (\emph{mm})
#'
#' @return
#'  numeric vector of lengths measured in \emph{inches}
#'
#' @seealso
#'  \code{\link{mm}} for converting \emph{inches} to \emph{mm}
#'
#' @export
#'
#' @examples
#'  inch(c(25.4, 1))
#'  # [1] 1.00000000 0.03937008  # [inch]
#'
#'  ## unit test:
#'  stopifnot(round(inch(c(25.4, 1)), 8) == c(1.0, 0.03937008))
#'
inch <- function(x) x/25.4
