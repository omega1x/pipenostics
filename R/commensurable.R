#' @title
#'   Check lengths of several vectors
#' @family Internal Utilities
#' @description
#'   The function is aimed to test whether several vectors being arguments of a
#'   function are commensurable just to strictly prevent wrong values on the
#'   function input instead of default warnings. This function should be used in
#'   conjunction with \code{\link[checkmate]{assert_true}}.
#' @param lengths
#'   a set of vector lengths to compare.
#'   Type: \code{\link[checkmate]{assert_integer}}.
#' @return
#'   Length validation status: single \code{TRUE}- or \code{FALSE}-value for
#'   success or fail appropriately.
#' @noRd
commensurable <- function(lengths) {
  checkmate::assert_integer(
    lengths, lower = 1L, any.missing = FALSE, min.len = 2
  )
  x <- lengths[lengths > 1L]
  length(x) < 2L || !(stats::var(x) > 0)
}
