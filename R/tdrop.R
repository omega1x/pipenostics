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
#'  entrance of pipe, [\emph{°C}], numeric vector
#'
#' @param pressure
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'  of heat carrier (water) inside the pipe, [\emph{MPa}], numeric vector
#'
#' @param consumption
#'  amount of heat carrier (water) that is transferred by pipe during a period, [\emph{ton/hour}], numeric vector
#'
#' @param flux
#'  heat flux emitted by pipe during a period, [\emph{kcal/hour}], numeric vector
#'
#' @return
#'  temperature drop at the outlet of pipe, [\emph{°C}], numeric vector
#'
#' @details
#'   Specific isobaric \href{https://en.wikipedia.org/wiki/Heat_capacity}{heat capacity}
#'   used in calculations is calculated according to
#'   \href{http://www.iapws.org/relguide/IF97-Rev.pdf}{IAPWS R7-97(2012)}
#'   for \strong{Region 1} since it is assumed that state of water in
#'   \emph{district heating system} is always in that region.
#'
#' @seealso
#'  \code{pdrop} for calculating pressure drop in pipe
#'
#' @export
#'
#' @examples
#'  # Calculate normative temperature drop for pipe
#'  pipeline <- list(
#'    year = 1968,
#'    laying = "channel",
#'    d = 700,
#'    l = 1000
#'  )
#'  operation_temperature <- 130
#'
#'  tdrop(
#'    temperature = operation_temperature,
#'    flux = do.call(
#'      m325nhl,
#'      c(pipeline, temperature = operation_temperature)
#'    )
#'  )
#'
#'
#'  # 1.37 [°C]

tdrop <- function(
  temperature = 130,
  pressure = mpa_kgf(6),
  consumption = 250,
  flux = 7000
  ){

  checkmate::assert_double(
    temperature, lower = 0, upper = 350, finite = TRUE,  any.missing = FALSE
  )
  checkmate::assert_double(
    pressure, lower = 8.4e-2, upper = 100, finite = TRUE, any.missing = FALSE
  )
  checkmate::assert_double(
    consumption, lower = 1e-3, upper = 1e5, finite = TRUE, any.missing = FALSE
  )
  checkmate::assert_double(
    flux, lower = 0, finite = TRUE, any.missing = FALSE
  )
  JOULE <- 0.2388458966  # [cal/J]
  pipe_heat_loss <- flux/JOULE  # [kJ/hour]
  g <- consumption * 1e3  # [kg/hour]
  pipe_heat_loss / g / cp1_tp(temperature + 273.15, pressure)  # [°C]=[°K]
}

