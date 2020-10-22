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

## * future if97 package*

## Region 1
if97t2 <- read.csv(
  text =
    "i,I,J,n
1,0,-2,0.14632971213167
2,0,-1,-0.84548187169114
3,0,0,-3.756360367204
4,0,1,3.3855169168385
5,0,2,-0.95791963387872
6,0,3,0.15772038513228
7,0,4,-0.016616417199501
8,0,5,0.00081214629983568
9,1,-9,0.00028319080123804
10,1,-7,-0.00060706301565874
11,1,-1,-0.018990068218419
12,1,0,-0.032529748770505
13,1,1,-0.021841717175414
14,1,3,-5.283835796993e-05
15,2,-3,-0.00047184321073267
16,2,0,-0.00030001780793026
17,2,1,4.7661393906987e-05
18,2,3,-4.4141845330846e-06
19,2,17,-7.2694996297594e-16
20,3,-4,-3.1679644845054e-05
21,3,0,-2.8270797985312e-06
22,3,6,-8.5205128120103e-10
23,4,-5,-2.2425281908e-06
24,4,-2,-6.5171222895601e-07
25,4,10,-1.4341729937924e-13
26,5,-8,-4.0516996860117e-07
27,8,-11,-1.2734301741641e-09
28,8,-6,-1.7424871230634e-10
29,21,-29,-6.8762131295531e-19
30,23,-31,1.4478307828521e-20
31,29,-38,2.6335781662795e-23
32,30,-39,-1.1947622640071e-23
33,31,-40,1.8228094581404e-24
34,32,-41,-9.3537087292458e-26"
)

cp1_tp <- function(temperature, pressure){
  checkmate::assert_double(temperature, lower = 273.15, upper = 623.15, any.missing = FALSE)
  checkmate::assert_double(pressure, upper = 100, any.missing = FALSE)
  checkmate::assert_true(all(pressure >= ps_t(temperature)))

  R <- 0.461526  # [kJ/kg/K]
  pii <- pressure/16.53
  tau <- 1386/temperature

  with(if97t2, {
    f <- function(pi_value, tau_value)
      sum( n*(7.1 - pi_value)^I*J*(J - 1)*(tau_value - 1.222)^(J - 2) )
    mapply(f, pii, tau)
  }) * R * -tau^2
}

## Saturation curve:
ps_t <- function(temperature){
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


stopifnot(
  all.equal(
    ps_t(c(300, 500, 600)),
    c(.353658941e-2, .263889776e1, .123443146e2)
  ),
  all.equal(
    cp1_tp(c(300, 300, 500), c(3, 80, 3)),
    c(.417301218, .401008987, 0.465580682)*10)
)
