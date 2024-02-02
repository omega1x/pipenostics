#' @title
#'  IAPWS-IF97. Specific isobaric heat capacity of water
#'
#' @family Fluid properties
#'
#' @description
#'  Estimate specific isobaric heat capacity of water in accordance with
#'  \href{http://www.iapws.org/relguide/IF97-Rev.pdf}{IAPWS-IF97},
#'  \emph{Region 1}, as a function of temperature and pressure.
#'
#' @param temperature
#'  temperature of water, [\emph{K}]. Type: \code{\link{assert_double}}.
#'
#' @param pressure
#'  pressure, [MPa]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  Specific isobaric heat capacity of water, [kJ/kg/K], [kJ/kg/ºC].
#'  Type: \code{\link{assert_double}}.
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
#'  if97cptp1(temperature = k_c(65), pressure = mpa_kgf(6))
#'  # [1] 4.184094  # [kJ/kg/K] = [kJ/kg/ºC]

if97cptp1 <- function(temperature = c(300, 300, 500), pressure = c(3, 80, 3)){
  checkmate::assert_double(
    temperature, lower = 273.15, upper = 623.15, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    pressure, upper = 100, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(all.commensurable(c(
    length(temperature), length(pressure)
  )))

  checkmate::assert_true(all(pressure >= if97pt4(temperature)))

  ## IAPWS Industrial Formulation 1997 for the
  ## Thermodynamic Properties of Water and Steam (IAPWS-IF97)
  ## [http://www.iapws.org/relguide/IF97-Rev.pdf]
  ##
  ## Region 1. Table 2
  if97t2 <- data.frame(
    i = 1:34,
    I = c(
       0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L,  1L,  1L,  2L,  2L,
       2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 8L, 8L, 21L, 23L, 29L, 30L, 31L,
      32L
    ),
    J = c(
       -2L, -1L,   0L,   1L,   2L,   3L, 4L, 5L, -9L, -7L, -1L,  0L,   1L,  3L,
       -3L,  0L,   1L,   3L,  17L,  -4L, 0L, 6L, -5L, -2L, 10L, -8L, -11L, -6L,
      -29L,-31L, -38L, -39L, -40L, -41L
    ),
    n = c(
       0.14632971213167000, -0.84548187169114000, -3.75636036720400000,
       3.38551691683850000, -0.95791963387872000,  0.15772038513228000,
      -0.01661641719950100,  0.00081214629983568,  0.00028319080123804,
      -0.00060706301565874, -0.01899006821841900, -0.03252974877050500,
      -0.02184171717541400, -5.2838357969930e-05, -0.00047184321073267,
      -0.00030001780793026,  4.7661393906987e-05, -4.4141845330846e-06,
      -7.2694996297594e-16, -3.1679644845054e-05, -2.8270797985312e-06,
      -8.5205128120103e-10, -2.2425281908000e-06, -6.5171222895601e-07,
      -1.4341729937924e-13, -4.0516996860117e-07, -1.2734301741641e-09,
      -1.7424871230634e-10, -6.8762131295531e-19,  1.4478307828521e-20,
       2.6335781662795e-23, -1.1947622640071e-23,  1.8228094581404e-24,
      -9.3537087292458e-26
    )
  )

  R <- 0.461526  # [kJ/kg/K]
  pii <- pressure/16.53
  tau <- 1386/temperature
  tau1 <- tau - 1.222
  pii1 <- 7.1 - pii

  # Use expanded expression to keep native vector operation for speed:
  with(if97t2, {
    # gamma_tau_tau, Region 1:
      n[ 1]*J[1]*(J[1] - 1)*tau1^(J[1] - 2) +
      n[ 2]*J[2]*(J[2] - 1)*tau1^(J[2] - 2) +
      n[ 5]*J[5]*(J[5] - 1) +
      n[ 6]*J[6]*(J[6] - 1)*tau1^(J[6] - 2) +
      n[ 7]*J[7]*(J[7] - 1)*tau1^(J[7] - 2) +
      n[ 8]*J[8]*(J[8] - 1)*tau1^(J[8] - 2) +
      n[ 9]*pii1^I[ 9]*J[9]*(J[9] - 1)*tau1^(J[9] - 2) +
      n[10]*pii1^I[10]*J[10]*(J[10] - 1)*tau1^(J[10] - 2) +
      n[11]*pii1^I[11]*J[11]*(J[11] - 1)*tau1^(J[11] - 2) +
      n[14]*pii1^I[14]*J[14]*(J[14] - 1)*tau1^(J[14] - 2) +
      n[15]*pii1^I[15]*J[15]*(J[15] - 1)*tau1^(J[15] - 2) +
      n[18]*pii1^I[18]*J[18]*(J[18] - 1)*tau1^(J[18] - 2) +
      n[19]*pii1^I[19]*J[19]*(J[19] - 1)*tau1^(J[19] - 2) +
      n[20]*pii1^I[20]*J[20]*(J[20] - 1)*tau1^(J[20] - 2) +
      n[22]*pii1^I[22]*J[22]*(J[22] - 1)*tau1^(J[22] - 2) +
      n[23]*pii1^I[23]*J[23]*(J[23] - 1)*tau1^(J[23] - 2) +
      n[24]*pii1^I[24]*J[24]*(J[24] - 1)*tau1^(J[24] - 2) +
      n[25]*pii1^I[25]*J[25]*(J[25] - 1)*tau1^(J[25] - 2) +
      n[26]*pii1^I[26]*J[26]*(J[26] - 1)*tau1^(J[26] - 2) +
      n[27]*pii1^I[27]*J[27]*(J[27] - 1)*tau1^(J[27] - 2) +
      n[28]*pii1^I[28]*J[28]*(J[28] - 1)*tau1^(J[28] - 2) +
      n[29]*pii1^I[29]*J[29]*(J[29] - 1)*tau1^(J[29] - 2) +
      n[30]*pii1^I[30]*J[30]*(J[30] - 1)*tau1^(J[30] - 2) +
      n[31]*pii1^I[31]*J[31]*(J[31] - 1)*tau1^(J[31] - 2) +
      n[32]*pii1^I[32]*J[32]*(J[32] - 1)*tau1^(J[32] - 2) +
      n[33]*pii1^I[33]*J[33]*(J[33] - 1)*tau1^(J[33] - 2) +
      n[34]*pii1^I[34]*J[34]*(J[34] - 1)*tau1^(J[34] - 2)
  }) * R * -tau^2
}


