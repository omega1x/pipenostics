#' @title
#'  Inches to mm
#'
#' @family utils
#'
#' @description
#'  Convert length measured in \href{https://en.wikipedia.org/wiki/Inch}{inches}
#'  to \href{https://en.wikipedia.org/wiki/Millimetre}{millimeters} (mm)
#'
#' @param x
#'  numeric vector of lengths measured in \emph{inches}
#'
#' @return
#'  numeric vector of lengths measured in \emph{millimeters} (\emph{mm})
#'
#' @seealso
#'  \code{\link{inch}} for converting \emph{mm} to \emph{inches}
#'
#' @export
#'
#' @examples
#'  mm(c(0.03937008, 1))
#'  # [1]  1.0 25.4  # [mm]
#'
#'  ## unit test:
#'  stopifnot(round(mm(c(0.03937008, 1)), 1) == c(1.0, 25.4))
#'
mm <- function(x) 25.4*x
