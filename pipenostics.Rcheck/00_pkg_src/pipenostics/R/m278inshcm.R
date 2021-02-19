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
#'  temperature of heat carrier (water) inside the pipe, [\emph{°C}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param material
#'  designation of insulation material as it stated in \code{\link{m278insdata}},
#'  Type: \code{\link{assert_subset}}.
#'
#' @return
#'  Thermal conductivity of insulation materials [\emph{W/m/°C}] at given
#'  set of temperatures. Type: \code{\link{assert_double}}.
#'
#' @export
#'
#' @examples
#'
#' # Averaged thermal conductivity of pipe insulation at 110 °C
#' print(m278insdata)
#' head(m278inshcm(110, m278insdata[["material"]]))
#' # [1] 0.09600 0.07525 0.14950 0.14325 0.14950 0.10800
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
                             any.missing = FALSE, min.len = 1)
    norms <- pipenostics::m278insdata
    checkmate::assert_subset(material, choices = norms$material)

      1e-3*norms[norms$material == material, "lambda"] +
        1e-6*norms[norms$material == material, "k"] * .5 * (temperature + 40)
  }
