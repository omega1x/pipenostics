#' @title
#'  Minenergo-325. Local heat loss coefficient
#'
#' @family Minenergo
#'
#' @description
#'  Calculate \eqn{\beta} - \emph{local heat loss coefficient} according to rule \emph{11.3.3}
#'  of \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'  \emph{Local heat loss coefficient} is used to increase normative heat losses
#'  of pipe by taking into account heat losses of fittings (shut-off valves,
#'  compensators and supports). This coefficient is applied mostly as a factor
#'  during the summation of heat losses of pipes in pipeline leveraging
#'  formula 14 of \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' @param laying
#'  type of pipe laying depicting the position of pipe in space:
#'  \itemize{
#'    \item \code{air},
#'    \item \code{channel},
#'    \item \code{room},
#'    \item \code{tunnel},
#'    \item \code{underground},
#'  }
#'  character vector.
#'
#' @param d
#'   internal diameter of pipe, [\emph{mm}], numeric vector.
#'
#' @return
#'  Two possible values of \eqn{\beta}: \code{1.2} or \code{1.15} depending on
#'  pipe laying and its diameter (numeric vector).
#'
#' @export
#'
#' @examples
#' norms <- within(m325nhldata, {
#'   beta <- m325beta(laying, as.double(diameter))
#' })
#'
m325beta <- function(laying = "channel", d = 700){
  norms <- pipenostics::m325nhldata
  checkmate::assert_double(d, lower = min(norms$diameter),
                           upper = max(norms$diameter),
                           finite = TRUE, any.missing = FALSE)
  checkmate::assert_subset(laying, choices = unique(norms$laying),
                           empty.ok = FALSE)
  type <- "channel"
  1.2*(d < 150 & laying == type) + 1.15*(laying != type | d >= 150)
}

