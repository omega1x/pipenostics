#' @title
#'  IAPWS-IF97. Saturation pressure of water (steam)
#'
#' @family Fluid properties
#'
#' @description
#'  Estimate saturation pressure of water (steam) in accordance with
#'  \href{http://www.iapws.org/relguide/IF97-Rev.pdf}{IAPWS-IF97},
#'  \emph{Region 4}, as a function of temperature.
#'
#' @param temperature
#'  temperature of water (steam), [\emph{K}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  Saturation pressure of water (steam), [MPa].Type: \code{\link{assert_double}}.
#'
#' @details
#'  Even the provided function is vectorized its execution may be too slow for
#'  large datasets. In that case use specialized packages for estimating
#'  water-steam properties with IAPWS formulations.
#'
#' @references
#'   IAPWS Industrial Formulation 1997 for the Thermodynamic Properties of
#'   Water and Steam (\href{http://www.iapws.org/relguide/IF97-Rev.pdf}{IAPWS-IF97}).
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  # Typical usage in district heating:
#'  if97pt4(temperature = k_c(65))
#'  # [1] 0.0250411  # [MPa]

if97pt4 <- function(temperature = c(300, 500, 600)){
  checkmate::assert_double(
    temperature, lower = 273.15, upper = 647.096, any.missing = FALSE,
    min.len = 1L
  )
  n <- c(
    0.11670521452767e4, -0.72421316703206e6, -0.17073846940092e2,
    0.12020824702470e5, -0.32325550322333e7,  0.14915108613530e2,
   -0.48232657361591e4,  0.40511340542057e6, -0.23855557567849,
    0.65017534844798e3
  )
  v <- temperature + n[9]/(temperature - n[10])
  A <-    1*v^2 + n[1]*v + n[2]
  B <- n[3]*v^2 + n[4]*v + n[5]
  C <- n[6]*v^2 + n[7]*v + n[8]
  (2*C/(-B + sqrt(B^2 - 4*A*C)))^4
}
