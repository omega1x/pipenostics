#' @title
#'  Minenergo-325. Temperature drop in pipe due heat losses
#'
#' @family Minenergo
#'
#' @description
#'  Calculate temperature drop in steel pipe of \emph{district heating system}
#'  (where water is a heat carrier) that is a result of heat losses through
#'  pipe wall and insulation using
#'  \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325} as a
#'  basis for values of heating flux.
#'
#'  Since \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325} is
#'  used as the basis for values of heating flux the calculated temperature
#'  drop may be considered as a \emph{normative temperature drop}. If the actual
#'  (somehow measured) temperature drop is more than this
#'  \emph{normative temperature drop} they may consider such difference to be
#'  due to \emph{extra-normative heat losses}. The presence of the latter
#'  requires appropriate maintenance activities.
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe measured at the
#'  entrance of pipe, [\emph{°C}]. Type: \code{\link{assert_double}}.
#'
#' @param pressure
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'  of heat carrier (water) inside the pipe, [\emph{MPa}]. Type: \code{\link{assert_double}}.
#'
#' @param consumption
#'  amount of heat carrier (water) that is transferred by pipe during a period,
#'  [\emph{ton/hour}]. Type: \code{\link{assert_double}}.
#'
#' @param d
#'  internal diameter of pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @param len
#'  length of pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param year
#'   year when the pipe is put in operation after laying or total overhaul.
#'   Type: \code{\link{assert_integerish}}.
#'
#' @param insulation
#'  insulation that covers the exterior of pipe:
#'  \describe{
#'    \item{\code{0}}{no insulation}
#'    \item{\code{1}}{foamed polyurethane or analogue}
#'    \item{\code{2}}{polymer concrete}
#'  }
#'  Type: \code{\link{assert_subset}}.
#'
#' @param laying
#'  type of pipe laying depicting the position of pipe in space:
#'  \itemize{
#'    \item \code{air},
#'    \item \code{channel},
#'    \item \code{room},
#'    \item \code{tunnel},
#'    \item \code{underground}.
#'  }
#'  Type: \code{\link{assert_subset}}.
#'
#' @param beta
#'  should they consider additional heat losses of fittings?
#'  Type: \code{\link{assert_logical}}.
#'
#' @param exp5k
#'  pipe regime flag: is pipe operated more that 5000 hours per year?
#'  Type: \code{\link{assert_logical}}.
#'
#' @return
#'  \emph{normative temperature drop} at the outlet of pipe, [\emph{°C}].
#'  Type: \code{\link{assert_double}}.
#'
#' @details
#'  The function is a simple wrapper for call of \code{\link{dropt}}
#'  with parameter \code{flux} calculated by \code{\link{m325nhl}}.
#'
#' @seealso
#'  \code{\link{dropt}} for calculating temperature drop in pipe using
#'  actual heat flux values
#'
#' @examples
#'  stopifnot(
#'    round(
#'      m325dropt(
#'        temperature = 130, year = 1968, laying = "channel", d = 700, l = 1000
#'      ), 2) == 1.37
#'  )
#'@export

m325dropt <- function(
  temperature = 130, pressure = mpa_kgf(6), consumption = 250,
  d = 700, # [mm]
  len = 1, # [m]
  year = 1986,
  insulation = 0,
  laying = "underground",
  beta = FALSE,
  exp5k = TRUE
  ){
  norms <- pipenostics::m325nhldata
  checkmate::assert_double(
    temperature, lower = 0, upper = 350, finite = TRUE,  any.missing = FALSE,
    min.len = 1
  )
  checkmate::assert_double(
    pressure, lower = 8.4e-2, upper = 100, finite = TRUE, any.missing = FALSE,
    min.len = 1
  )
  checkmate::assert_double(
    consumption, lower = 1e-3, upper = 1e5, finite = TRUE, any.missing = FALSE,
    min.len = 1
  )
  checkmate::assert_double(d, lower = min(norms$diameter),
                           upper = max(norms$diameter),
                           finite = TRUE, any.missing = FALSE,
                           min.len = 1)
  checkmate::assert_double(len, lower = 0, finite = TRUE, any.missing = FALSE,
                           min.len = 1)
  checkmate::assert_integerish(year, lower = 1900L,
                               upper = max(norms$epoch),
                               any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(insulation, choices = unique(norms$insulation))
  checkmate::assert_subset(laying, choices = unique(norms$laying),
                           empty.ok = FALSE)
  checkmate::assert_logical(beta, any.missing = FALSE, min.len = 1)
  checkmate::assert_logical(exp5k, any.missing = FALSE, min.len = 1)

  duration <- 1  # [hour]
  extra <- 2
  flux <- pipenostics::m325nhl(
    year, laying, exp5k, insulation, d,
    temperature, len, duration, beta, extra
  )  # [kcal/hour]
  pipenostics::dropt(temperature, pressure, consumption, flux)
}