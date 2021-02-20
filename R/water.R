#' Physical properties of waters and fluids
#'
#' Sources:
#'   * IAPWS R7-97(2012) (http://www.iapws.org/relguide/IF97-Rev.pdf)
#'   * IAPWS R12-08 (http://www.iapws.org/relguide/visc.pdf)
#'
#' @noRd

## IAPWS R7-97(2012). Region 1
## table 2
r97t2 <- read.csv(
  text =
    "i,I,J,n
1,0,-2,.14632971213167
2,0,-1,-.84548187169114
3,0,0,-3.756360367204
4,0,1,3.3855169168385
5,0,2,-.95791963387872
6,0,3,.15772038513228
7,0,4,-.016616417199501
8,0,5,.81214629983568e-3
9,1,-9,.28319080123804e-3
10,1,-7,-.60706301565874e-3
11,1,-1,-.018990068218419
12,1,0,-.032529748770505
13,1,1,-.021841717175414
14,1,3,-5.283835796993e-5
15,2,-3,-.47184321073267e-3
16,2,0,-.30001780793026e-3
17,2,1,4.7661393906987e-5
18,2,3,-4.4141845330846e-6
19,2,17,-7.2694996297594e-16
20,3,-4,-3.1679644845054e-5
21,3,0,-2.8270797985312e-6
22,3,6,-8.5205128120103e-10
23,4,-5,-2.2425281908e-6
24,4,-2,-6.5171222895601e-7
25,4,10,-1.4341729937924e-13
26,5,-8,-4.0516996860117e-7
27,8,-11,-1.2734301741641e-9
28,8,-6,-1.7424871230634e-10
29,21,-29,-6.8762131295531e-19
30,23,-31,1.4478307828521e-20
31,29,-38,2.6335781662795e-23
32,30,-39,-1.1947622640071e-23
33,31,-40,1.8228094581404e-24
34,32,-41,-9.3537087292458e-26"
)

## IAPWS R7-97(2012). Specific isobaric heat capacity of water,
## [kJ/kg/K] == [kJ/kg/ºC]
cp1_tp <- function(temperature = c(300, 300, 500), pressure = c(3, 80, 3)){
  checkmate::assert_double(temperature, lower = 273.15, upper = 623.15, any.missing = FALSE)
  checkmate::assert_double(pressure, upper = 100, any.missing = FALSE)
  checkmate::assert_true(all(pressure >= ps_t(temperature)))

  R <- 0.461526  # [kJ/kg/K]
  pii <- pressure/16.53
  tau <- 1386/temperature
  tau1 <- tau - 1.222
  pii1 <- 7.1 - pii

  # Use expanded expression to keep native vector operation for speed:
  with(r97t2, {
    # gamma_tau_tau, Region 1:
    n[1]*J[1]*(J[1] - 1)*tau1^(J[1] - 2) +
      n[2]*J[2]*(J[2] - 1)*tau1^(J[2] - 2) +
      n[5]*J[5]*(J[5] - 1) +
      n[6]*J[6]*(J[6] - 1)*tau1^(J[6] - 2) +
      n[7]*J[7]*(J[7] - 1)*tau1^(J[7] - 2) +
      n[8]*J[8]*(J[8] - 1)*tau1^(J[8] - 2) +
      n[9]*pii1^I[9]*J[9]*(J[9] - 1)*tau1^(J[9] - 2) +
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

## IAPWS R7-97(2012). Water pressure on saturation curve, [MPa]
ps_t <- function(temperature = c(300, 500, 600)){
  checkmate::assert_double(temperature, lower = 273.15, upper = 647.096)
  n <- c( 0.11670521452767e4, -0.72421316703206e6, -0.17073846940092e2,
          0.12020824702470e5, -0.32325550322333e7,  0.14915108613530e2,
          -0.48232657361591e4,  0.40511340542057e6, -0.23855557567849,
          0.65017534844798e3)
  v <- temperature + n[9]/(temperature - n[10])
  A <-    1*v^2 + n[1]*v + n[2]
  B <- n[3]*v^2 + n[4]*v + n[5]
  C <- n[6]*v^2 + n[7]*v + n[8]
  (2*C/(-B + sqrt(B^2 - 4*A*C)))^4
}


# IAPWS R7-97(2012). Specific volume, [m^3/kg]:
v1_tp <- function(temperature = c(300, 300, 500), pressure = c(3, 80, 3)) {
  checkmate::assert_double(temperature, lower = 273.15, upper = 623.15,
                           any.missing = FALSE)
  checkmate::assert_double(pressure, upper = 100, any.missing = FALSE)
  checkmate::assert_true(all(pressure >= ps_t(temperature)))

  R <- 0.461526  # [kJ/kg/K]
  pii <- pressure/16.53
  tau <- 1386/temperature
  tau1 <- tau - 1.222
  pii1 <- 7.1 - pii

  # Use expanded expression to keep native vector operation for speed:
  with(r97t2, {
    # gamma_pi, Region 1:
    -n[ 9]*tau1^J[ 9] +
      -n[10]*tau1^J[10] +
      -n[11]*tau1^J[11] +
      -n[12] +
      -n[13]*tau1^J[13] +
      -n[14]*tau1^J[14] +

      -n[15]*I[15]*pii1*tau1^J[15] +
      -n[16]*I[16]*pii1 +
      -n[17]*I[17]*pii1*tau1^J[17] +
      -n[18]*I[18]*pii1*tau1^J[18] +
      -n[19]*I[19]*pii1*tau1^J[19] +

      -n[20]*I[20]*pii1^(I[20] - 1)*tau1^J[20] +
      -n[21]*I[21]*pii1^(I[21] - 1) +
      -n[22]*I[22]*pii1^(I[22] - 1)*tau1^J[22] +
      -n[23]*I[23]*pii1^(I[23] - 1)*tau1^J[23] +
      -n[24]*I[24]*pii1^(I[24] - 1)*tau1^J[24] +
      -n[25]*I[25]*pii1^(I[25] - 1)*tau1^J[25] +
      -n[26]*I[26]*pii1^(I[26] - 1)*tau1^J[26] +
      -n[27]*I[27]*pii1^(I[27] - 1)*tau1^J[27] +
      -n[28]*I[28]*pii1^(I[28] - 1)*tau1^J[28] +
      -n[29]*I[29]*pii1^(I[29] - 1)*tau1^J[29] +
      -n[30]*I[30]*pii1^(I[30] - 1)*tau1^J[30] +
      -n[31]*I[31]*pii1^(I[31] - 1)*tau1^J[31] +
      -n[32]*I[32]*pii1^(I[32] - 1)*tau1^J[32] +
      -n[33]*I[33]*pii1^(I[33] - 1)*tau1^J[33] +
      -n[34]*I[34]*pii1^(I[34] - 1)*tau1^J[34]
  }) * R * temperature/16.53 * 1e-3  # Warning! No 1e-3 in IF97 document!
}

stopifnot(
  all.equal(
    v1_tp(),
    c(.100215168e-2, .971180894e-3, .120241800e-2)
  )
)


## IAPWS R12-08.Dynamic viscosity
## table 2
r12t2 <- read.csv(text =
                    "i,j,Hij
0,0,5.20094e-1
1,0,8.50895e-2
2,0,-1.08374
3,0,-2.89555e-1
0,1,2.22531e-1
1,1,9.99115e-1
2,1,1.88797
3,1,1.26613
5,1,1.20573e-1
0,2,-2.81378e-1
1,2,-9.06851e-1
2,2,-7.72479e-1
3,2,-4.89837e-1
4,2,-2.57040e-1
0,3,1.61913e-1
1,3,2.57399e-1
0,4,-3.25372e-2
3,4,6.98452e-2
4,5,8.72102e-3
3,6,-4.35673e-3
5,6,-5.93264e-4"
)

## IAPWS R12-08. Dynamic viscosity
## formulas (10)-(12)
dynvisc <- function(temperature, density){
  checkmate::assert_double(temperature, lower = 273.15, upper = 1173.15,
                           finite = TRUE, any.missing = FALSE)
  checkmate::assert_double(density, lower = 1, upper = 1200,
                           finite = TRUE, any.missing = FALSE)
  tau <- temperature/647.096  # [] = [K]/[K]
  rho <- density/322  # [] = [kg/m^3]/[kg/m^3]
  mu0 <- 100*sqrt(tau)/(1.67752 + 2.20462/tau + 0.6366564/tau^2 - 0.241605/tau^3)
  z <- as.matrix(r12t2)
  n <- list(i = 6, j = 7)
  h <- matrix(0, n$i, n$j)
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

## IAPWS R12-08. Dynamic viscosity
## table 4, validation data
r12t4 <- read.csv(text =
                    #[ºC]        [kg/m^3][Pa*s]*1e-6
                    "temperature,density,dynvisc
298.15,998.,889.735100
298.15,1200.,1437.649467
373.15,1000.,307.883622
433.15,1.,14.538324
433.15,1000.,217.685358
873.15,1.,32.619287
873.15,100.,35.802262
873.15,600.,77.430195
1173.15,1.,44.217245
1173.15,100.,47.640433
1173.15,400.,64.154608"
)

## Reynolds number
## (https://en.wikipedia.org/wiki/Reynolds_number)
re_r <- function(d, mu, rho, u){
  d/mu * rho*u  ## [m]/[kg/m/s] * [kg/m^3] * [m/s] = []
}

# re_m <- function(d, mu, m, a = .25*pi*d^2){
#   d/mu/a * m  ## [m]/[kg/m/s]/[m^2] * [kg/s] = []
# }
#
# re_v <- function(d, nu, v, a = .25*pi*d^2){
#   d/nu/a * v  ## [m]/[m^2/s]/[m^2] * [m^3/s] = []
# }
#
# re_u <- function(d, nu, u){
#   d/nu * u  ## [m]/[m^2/s] * [m/s] = []
# }

## Friction factors as a function of Reynolds number and relative roughness:
##
## Use the next assertions for `fric_*`-functions when embedding in code:
##  checkmate::assert_double(reynolds, lower = 1, finite = TRUE,
##                          any.missing = FALSE)
##  checkmate::assert_double(roughness, lower = 0,
##                           upper = .2,  # According to Moody chart
##                           any.missing = FALSE)

## Romeo, Royo and Monzón approximation of Colebrook equation
## Reference:
##   Romeo, E., Royo, C., Monzon, A., 2002. Improved explicit equation for
##   estimation of the friction factor in rough and smooth pipes.
##   Chem. Eng. J. 86 (3), 369–374.
fric_romeo <- function(reynolds, roughness = 0) {
  C <- (roughness/7.7918)^.9924 + (5.3326/(208.815 + reynolds))^.9345
  B <- roughness/3.827 - 4.567/reynolds*log10(C)
  A <- roughness/3.7065 - 5.0272/reynolds*log10(B)
  .25/log10(A)^2
}

## Vatankhan and Kouchakzadeh rearrangement of Sonnad approximation of Colebrook
## equation
## Reference:
##   Vatankhah, A.R., Kouchakzadeh, S., 2009. Discussion: Exact equations for
##   pipeflow problems, by P.K. Swamee and P.N. Rathie. J. Hydraul. Res. IAHR 47 (7),
##   537–538.
fric_vatankhan <- function(reynolds, roughness = 0) {
  G <- .124*reynolds*roughness + log(.4587*reynolds)
  (.8686*log(.4587*reynolds/(G - .31)^(G/(G + .9633))))^-2

}


## Buzelli approximation of Colebrook equation
## Reference:
##   Buzzelli, D., 2008. Calculating friction in one step. Mach. Des. 80 (12), 54–55.
fric_buzelli <- function(reynolds, roughness = 0) {
  B1 <- (.774*log(reynolds) - 1.41)/(1 + 1.32*sqrt(roughness))
  B2 <- roughness/3.7 * reynolds + 2.51*B1
  ( B1 - (B1 + 2*log10(B2/reynolds))/(1 + 2.18/B2) )^-2
}

