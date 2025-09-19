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
#'  its value varies from \emph{0.0} \emph{h⁻¹} (no loss of heat carrier)
#'  up to \emph{0.0025} \emph{h⁻¹} (maximum possible loss allowed). The
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
#'  [\emph{h⁻¹}]. Type: \code{\link[checkmate]{assert_double}}.
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
#'    \item{
#'      For \code{m325nvl}}{volume loss of heat carrier per hour, [\emph{m³/h}].
#'    }
#'    \item{
#'      For \code{m325nml}}{mass loss of heat carrier per hour, [\emph{ton/h}].
#'    }
#'  }
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # According to Minenergo-325 it may be granted to loose right up to the next
#' # value of tons of heat carrier (water) per year (nine-month heating season)
#' # for the typical supplying 100-meter length pipe:
#' m325nml(a = 0.0025, len = 100) * 24 * 90
#'
#' @rdname m325nvl
#' @export
m325nvl <- function(a = 0, d = 720, wth = 12, len = 1) {
  checkmate::assert_double(
    a,
    lower = 0, upper = 25e-4, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = min(pipenostics::m325nhldata[["d"]]),
    upper = max(pipenostics::m325nhldata[["d"]]),
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
  checkmate::assert_true(
    commensurable(c(length(a), length(d), length(wth), length(len)))
  )
  checkmate::assert_true(all(d - 2 * wth > 0.5))  # in mm

  METER <- 1e-3  # [m/mm]
  0.25 * a * base::pi * ((d - 2 * wth) * METER)^2 * len
}


#' @rdname m325nvl
#' @export
m325nml <- function(
  temperature = 130, pressure = mpa_kgf(6), a = 0, d = 720, wth = 12, len = 1
) {
  checkmate::assert_double(
    a,
    lower = 0, upper = 25e-4, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = min(pipenostics::m325nhldata[["d"]]),
    upper = max(pipenostics::m325nhldata[["d"]]),
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
  checkmate::assert_true(
    commensurable(c(length(a), length(d), length(wth), length(len)))
  )
  checkmate::assert_true(all(d - 2 * wth > 0.5))  # in mm

  METER <- 1e-3  # [m/mm]
  TON   <- 1e-3  # [ton/kg]

  rho <- unname(
    iapws::if97("rho", pressure, pipenostics::k_c(temperature))[, 1]
  ) * TON  # [ton]
  0.25 * a * base::pi * ((d - 2 * wth) * METER)^2 * len * rho  # [ton/h]
}
