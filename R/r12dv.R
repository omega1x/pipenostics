#' @title
#'  IAPWS R12-08. Dynamic viscosity of water (steam)
#'
#' @family Fluid properties
#'
#' @description
#'  Estimate dynamic (shear) viscosity of pure water substance over an extensive
#'  range of fluid states in accordance with
#'  \emph{Release on the IAPWS Formulation 2008 for the Viscosity of Ordinary Water Substance}
#'  (\href{http://www.iapws.org/relguide/visc.pdf}{IAPWS R12-08})
#'  as a function of temperature and density.
#'
#' @param temperature
#'  temperature of water (steam), [\emph{K}]. Type: \code{\link{assert_double}}.
#'
#' @param density
#'  density of water, [\emph{kg/m^3}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  Dynamic viscosity of water (steam), [\emph{Pa*s}]*1e-6.Type: \code{\link{assert_double}}.
#'
#' @details
#'  Even the provided function is vectorized its execution may be too slow for
#'  large datasets. In that case use specialized packages for estimating
#'  water-steam properties with IAPWS formulations.
#'
#' @references
#'   \href{http://www.iapws.org/relguide/visc.pdf}{IAPWS R12-08}.
#'   \emph{Release on the IAPWS Formulation 2008 for the Viscosity of Ordinary Water Substance}
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  # Typical usage in district heating:
#'  t <- 65 + 273.15
#'  rho <- 1/if97vtp1(t, mpa_kgf(6))
#'  r12dv(t, rho)
#'  # 433.0342  ## [Pa*s]*1e-6

r12dv <- function(temperature, density){
  checkmate::assert_double(temperature, lower = 273.15, upper = 1173.15,
                           finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(density, lower = 1, upper = 1200,
                           finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_true(all.commensurable(c(
    length(temperature), length(density)
  )))

  tau <- temperature/647.096  # [] = [K]/[K]
  rho <- density/322  # [] = [kg/m^3]/[kg/m^3]
  mu0 <- 100*sqrt(tau)/(1.67752 + 2.20462/tau + 0.6366564/tau^2 - 0.241605/tau^3)

  z <- structure(
    c(
      0, 1, 2, 3, 0, 1, 2, 3, 5, 0, 1, 2, 3, 4, 0, 1, 0, 3, 4, 3, 5,
      0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 6, 6,

       0.52009400,  0.08508950, -1.083740000, -0.289555,  0.22253100,  0.99911500,
       1.88797000,  1.26613000,  0.120573000, -0.281378, -0.90685100, -0.77247900,
      -0.48983700, -0.25704000,  0.161913000, 0.2573990, -0.03253720,  0.06984520,
       0.00872102, -0.00435673, -0.000593264
    ),
    dim = c(21L, 3L),
    dimnames = list(NULL, c("i", "j", "Hij"))
  )

  n <- list(i = 6, j = 7)
  h <- matrix(0, n[["i"]], n[["j"]])
  h[z[, c("i", "j")] + 1] <- z[, "Hij"]

  # Use expanded expression to keep native vector operation for speed:
  tau1 <- 1/tau - 1
  rho1 <- rho - 1
  rho2 <- rho1^2
  rho3 <- rho1^3
  rho4 <- rho1^4
  rho6 <- rho1^6

  mu1 <- exp(
    rho * (
      h[1, 1] + h[1, 2]*rho1 + h[1, 3]*rho2 + h[1, 4]*rho3 + h[1, 5]*rho4 +
        tau1*(h[2, 1] + h[2, 2]*rho1 +  h[2, 3]*rho2 + h[2, 4]*rho3) +
        tau1^2*(h[3, 1] + h[3, 2]*rho1 + h[3, 3]*rho2) +
        tau1^3*(h[4, 1] + h[4, 2]*rho1 + h[4, 3]*rho2 + h[4, 5]*rho4 + h[4, 7]*rho6) +
        tau1^4*(h[5, 3]*rho2 + h[5, 6] * rho1^5) +
        tau1^5*(h[6, 2]*rho1 + h[6, 7]*rho6)
    )
  )
  mu2 <- 1  # Since for industrial use
  mu0*mu1*mu2  # 1e-6*[Pa*s] == 1e-6*[N*s/m^2] == 1e-6*[kg/m/s]
}
