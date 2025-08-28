#' @title
#'  Minenergo-325. Normative material loss of pipe
#'
#' @family Minenergo
#'
#' @description
#'  Calculate normative material loss of heat carrier (water) from
#'  cylindrical pipe according to rule \emph{10.1.2} of
#'  \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' @details
#'  The calculations are based on the \emph{a}-factor, which may be treated as
#'  the maximum allowed rate of heat carrier (water) volume loss per hour. So,
#'  its value varies from \emph{0.0} \eqn{hour^{-1}} (no loss of heat carrier)
#'  up to \emph{0.0025} \eqn{hour^{-1}} (maximum possible loss allowed). The
#'  cylindrical form of pipe is always assumed in calculations.
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe, [\emph{°C}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param pressure
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute
#'  pressure} of heat carrier (water) inside the pipe, [\emph{MPa}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param a
#'  heat carrier (water) volume loss factor of cylindrical pipe,
#'  [\eqn{hour^{-1}}]. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param d
#'  nominal (outside) diameter of pipe, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param len
#'  pipe length, [\emph{m}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#'
#' @return
#'  \describe{
#'    \item{For \code{m325nvl}}{volume loss of heat carrier per hour, [\emph{m^3/hour}].}
#'    \item{For \code{m325nml}}{mass loss of heat carrier per hour, [\emph{ton/hour}].}
#'  }
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @examples
#'  library(pipenostics)
#'
#' @rdname m325nvl
#' @export
m325nvl <- function(a = 0, d = 720, wth = 12, len = 1) { # TODO: add examples for the function
  checkmate::assert_double(
    a,
    lower = 0, upper = 25e-4, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = min(pipenostics::m325nhldata[["diameter"]]),
    upper = max(pipenostics::m325nhldata[["diameter"]]),
    finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    wth,
    lower = min(pipenostics::b36pipedata[["wth"]]),
    upper = max(pipenostics::b36pipedata[["wth"]]),
    finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    len,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  commensurable(c(length(a), length(d), length(wth), length(len)))

  METER <- 1e-3  # [m/mm]
  0.25*a*pi*((d - 2*wth)*METER)^2*len
}


#' @rdname m325nvl
#' @export
m325nml <- function(temperature = 130, pressure = mpa_kgf(6), a = 0, d = 720, wth = 12, len = 1) { # TODO: add examples for the function
  checkmate::assert_double(
    a,
    lower = 0, upper = 25e-4, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = min(pipenostics::m325nhldata[["diameter"]]),
    upper = max(pipenostics::m325nhldata[["diameter"]]),
    finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    wth,
    lower = min(pipenostics::b36pipedata[["wth"]]),
    upper = max(pipenostics::b36pipedata[["wth"]]),
    finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    len,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  commensurable(c(length(a), length(d), length(wth), length(len)))

  METER <- 1e-3  # [m/mm]
  TON   <- 1e-3  # [ton/kg]

  rho <- unname(
    iapws::if97("rho", pressure, pipenostics::k_c(temperature))[, 1]
  )*TON  # [ton]
  0.25*a*pi*( (d - 2*wth)*METER )^2*len*rho  # [ton/hour]
}

