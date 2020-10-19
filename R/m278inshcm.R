#' @title
#'  Minenergo-278. Thermal conductivity of pipe insulation materials
#'
#' @family Minenergo
#'
#' @description
#'  Get normative values of thermal conductivity of pipe insulation
#'  materials affirmed by
#'  \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278} as
#'  a function of temperature of heat carrier (water).
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe, [\emph{°C}],
#'  numeric vector.
#'
#' @param material
#'  designation of insulation material as it stated in \code{\link{m278insdata}},
#'  character vector.
#'
#' @return
#'  Thermal conductivity of insulation materials [\emph{W/m/°C}] at given
#'  set of temperatures.
#'
#' @export
#'
#' @examples
#'
#' # Averaged thermal conductivity of pipe insulation at 110 °C
#' print(m278insdata)
#' stopifnot(
#'   round(
#'     mean(with(m278insdata, { m278inshcm(110, material)})),
#'     2) == 9e-2
#' )
#'
#' # Terms for linear connection between thermal conductivity of unknown
#' # (averaged) pipe insulator vs temperature:
#' temperature <- as.double(1:450)
#' lambda_ins <- with(m278insdata, {
#'   vapply(temperature, function(x) mean(m278inshcm(x, material)), .1)
#' })
#' C <- coef(lsfit(temperature, lambda_ins))  # c(Intercept, X)
#' stopifnot(
#'   all(abs(C - c(7.963590e-02, 9.730769e-05)) < 1e-8)
#' )
#'
m278inshcm <- function(temperature = 110, material = "aerocrete"){
    checkmate::assert_double(temperature, lower = 0, upper = 450, finite = TRUE,
                                                         any.missing = FALSE)
    norms <- pipenostics::m278insdata
    checkmate::assert_subset(material, choices = norms$material)

      1e-3*norms[norms$material == material, "lambda"] +
        1e-6*norms[norms$material == material, "k"] * .5 * (temperature + 40)
  }