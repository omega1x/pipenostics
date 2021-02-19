#' @title
#'  Temperature drop in pipe due heat losses
#'
#' @family district heating
#'
#' @description
#'  Calculate temperature drop in steel pipe of \emph{district heating system}
#'  (where water is a heat carrier) that is a result of heat losses through
#'  pipe wall and insulation.
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe measured at the
#'  inlet of pipe, [\emph{°C}]. Type: \code{\link{assert_double}}.
#'
#' @param pressure
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'  of heat carrier (water) inside the pipe, [\emph{MPa}]. Type: \code{\link{assert_double}}.
#'
#' @param consumption
#'  amount of heat carrier (water) that is transferred by pipe during a period,
#'  [\emph{ton/hour}]. Type: \code{\link{assert_double}}.
#'
#' @param flux
#'  heat flux emitted by pipe during a period, [\emph{kcal/hour}].
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  temperature drop at the outlet of pipe, [\emph{°C}]. Type: \code{\link{assert_double}}.
#'
#' @details
#'   Specific isobaric \href{https://en.wikipedia.org/wiki/Heat_capacity}{heat capacity}
#'   used in calculations is calculated according to
#'   \href{http://www.iapws.org/relguide/IF97-Rev.pdf}{IAPWS R7-97(2012)}
#'   for \strong{Region 1} since it is assumed that state of water in
#'   \emph{district heating system} is always in that region.
#'
#' @seealso
#'  \code{\link{m325dropt}} for calculating normative values of temperature drop
#'
#' @export
#'
#' @examples
#'  # Calculate normative temperature drop based on Minenergo-325 for pipe segment
#'  pipeline <- list(
#'    year = 1968,
#'    laying = "channel",
#'    d = 700,
#'    l = 1000
#'  )
#'  operation_temperature <- c(130, 150)  # [°C]
#'
#'  foo <- dropt(
#'    temperature = operation_temperature,
#'    flux = do.call(
#'      m325nhl,
#'      c(pipeline, temperature = list(operation_temperature))
#'    )
#'  )
#'
#'  foo
#'  # [1] 1.366806 1.433840
#'
#'  # This is the same as using m325dropt:
#'  bar <- m325dropt(temperature = operation_temperature,
#'    year = 1968, laying = "channel", d = 700, len = 1000
#'  )
#'
#'  bar
#'  # [1] 1.366806 1.433840
#'

dropt <- function(
  temperature = 130,
  pressure = mpa_kgf(6),
  consumption = 250,
  flux = 7000
  ){

  checkmate::assert_double(
    temperature, lower = 0, upper = 350, finite = TRUE,  any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    pressure, lower = 8.4e-2, upper = 100, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    consumption, lower = 1e-3, upper = 1e5, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  checkmate::assert_double(
    flux, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  JOULE <- 0.2388458966  # [cal/J]
  pipe_heat_loss <- flux/JOULE  # [kJ/hour]
  g <- consumption * 1e3  # [kg/hour]
  pipe_heat_loss / g / cp1_tp(temperature + 273.15, pressure)  # [°C]=[°K]
}

