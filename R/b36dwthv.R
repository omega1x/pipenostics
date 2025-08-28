#' @title
#'  Verify the alignment between nominal pipe diameter and wall thickness
#'
#' @family utils
#'
#' @description
#'  Check the correspondence inside nominal diameter/wall thickness value pair
#'  for pipes manufactured in accordance with regulatory standards.
#'
#'  Verification is performed using reference values from the
#'  \code{\link{b36pipedata}}-table.
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
#'  Verification status.Type: \code{\link[checkmate]{assert_logical}}.
#'
#' @examples
#'  library(pipenostics)
#'
#' @export
b36dwthv <- function(d, wth){  # TODO: write examples here
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

  interaction(d, wth, drop = TRUE) %in%
  interaction(
    pipenostics::b36pipedata[["d"]], pipenostics::b36pipedata[["wth"]],
    drop = TRUE
  )
}
