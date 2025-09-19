#' @title
#'  ASME B36.10M. Verify the alignment between nominal pipe diameter and wall
#'  thickness
#'
#' @family ASME B36.10M
#'
#' @description
#'  Check the correspondence inside nominal diameter/wall thickness value pair
#'  for pipes manufactured in accordance with regulatory standards.
#'
#'  Verification is performed using reference values from the
#'  \code{\link{b36pipedata}}-table. So, the result is \code{TRUE} if \code{d}-
#'  and \code{w}-value combination can be found in rows of this dataset.
#'
#' @param d
#'  nominal (outside) diameter of pipe, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  Verification status. Type: \code{\link[checkmate]{assert_logical}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # All pipes in test bench of district heating network have strictly
#' # consistent diameters and wall thicknesses:
#'
#' all(b36dwthv(m325nxdata[["d"]], m325nxdata[["wth"]]))
#'
#' @export
b36dwthv <- function(d, wth) {
  checkmate::assert_double(
    d,
    lower = min(pipenostics::b36pipedata[["d"]]),
    upper = max(pipenostics::b36pipedata[["d"]]),
    finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  n <- length(d)
  checkmate::assert_double(
    wth,
    lower = min(pipenostics::b36pipedata[["wth"]]),
    upper = max(pipenostics::b36pipedata[["wth"]]),
    finite = TRUE, any.missing = FALSE,
    len = n
  )
  checkmate::assert_true(commensurable(c(length(d), length(wth))))
  checkmate::assert_true(all(d - 2 * wth > 0.5))  # in mm

  interaction(d, wth, drop = TRUE) %in% {
    interaction(
      pipenostics::b36pipedata[["d"]], pipenostics::b36pipedata[["wth"]],
      drop = TRUE
    )
  }
}
