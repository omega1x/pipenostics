% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mepof.R
\name{mepof}
\alias{mepof}
\title{Probability of failure of the corroded pipe within maximum entropy}
\usage{
mepof(
  depth = seq(0, 10, length.out = 100),
  l = seq(40, 50, length.out = 100),
  d = rep.int(762, 100),
  wth = rep.int(10, 100),
  strength = rep.int(358.5274, 100),
  pressure = rep.int(0.588, 100),
  temperature = rep.int(150, 100),
  rar = function(n) stats::runif(n, 0.01, 0.3)/365,
  ral = function(n) stats::runif(n, 0.01, 0.3)/365,
  days = 0,
  k = 0.8,
  method = "b31g",
  n = 1e+06
)
}
\arguments{
\item{depth}{maximum depth of the corroded area measured during \emph{inline inspection},
[\emph{mm}]. Type: \code{\link{assert_double}}.}

\item{l}{maximum longitudinal length of corroded area measured during
\emph{inline inspection}, [\emph{mm}]. Type: \code{\link{assert_double}}.}

\item{d}{nominal outside diameter of pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.}

\item{wth}{nominal wall thickness of pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.}

\item{strength}{one of the next characteristics of steel strength, [\emph{MPa}]:
\itemize{
  \item specified minimum yield of stress (\emph{SMYS})
        for use with \code{\link{b31gpf}} and \code{\link{b31gmodpf}}.
  \item ultimate tensile strength (\emph{UTS}) or specified minimum tensile
        strength (\emph{SMTS}) for use with other failure pressure codes
        (\code{\link{dnvpf}}, \code{\link{pcorrcpf}}, \code{\link{shell92pf}}).
}
Type: \code{\link{assert_double}}.}

\item{pressure}{\href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
of substance (i.e. heat carrier) inside the pipe measured near defect
position, [\emph{MPa}]. In most cases this is a nominal operating pressure.
Type: \code{\link{assert_double}}.}

\item{temperature}{temperature of substance (i.e. heat carrier) inside the pipe measured near
defect position, [\emph{°C}]. In case of district heating network this is
usually a calculated value according to actual or normative thermal-hydraulic
regime. Type: \code{\link{assert_double}}.}

\item{rar}{random number generator for simulating of distribution of radial corrosion
rate in pipe wall, [\emph{mm/day}]. The only
argument \code{n} of the function should be the number of observations to
generate. Type: \code{\link{assert_function}}.}

\item{ral}{random number generator for simulating of distribution of longitudinal corrosion
rate in pipe wall, [\emph{mm/day}]. The only
argument \code{n} of the function should be the number of observations to
generate. Type: \code{\link{assert_function}}.}

\item{days}{number of days that have passed after or preceded the \emph{inline inspection}, [].
Negative values are for retrospective assumptions whereas positives are for
failure prognosis. Type: \code{\link{assert_int}}.}

\item{k}{alarm threshold for leakage failure. It usually \code{0.6}, \code{0.7}, or
\code{0.8}, []. If set to \code{1} no alarm before failure occurs.
Type: \code{\link{assert_number}}.}

\item{method}{method for calculating failure pressure:
\itemize{
  \item \emph{b31g} - using \code{\link{b31gpf}}.
  \item \emph{b31gmod} - using \code{\link{b31gmodpf}}.
  \item \emph{dnv} - using \code{\link{dnvpf}}.
  \item \emph{pcorrc} - using \code{\link{pcorrcpf}}.
  \item \emph{shell92} - using \code{\link{shell92pf}}.
}
Type: \code{\link{assert_choice}}.}

\item{n}{number of observations to generate for
\href{https://en.wikipedia.org/wiki/Monte_Carlo_method#Monte_Carlo_and_random_numbers}{Monte-Carlo simulations},
Type: \code{\link{assert_count}}.}
}
\value{
Probability of pipe failure for each corroded area measured during
  \emph{inline inspection}. Type: \code{\link{assert_double}}.
  If \code{NA}s returned use another method
  for calculating failure pressure.
}
\description{
Calculate \emph{probability of failure} (POF) of the corroded pipe taking into
 account its actual level of defectiveness and exploiting
 \href{https://en.wikipedia.org/wiki/Monte_Carlo_method#Monte_Carlo_and_random_numbers}{Monte-Carlo simulation}
 within \href{https://en.wikipedia.org/wiki/Principle_of_maximum_entropy}{Principle of maximum entropy}.

 Consistent estimate of POF for pipeline systems plays
 a critical role in optimizing their operation. To prevent pipeline failures
 due to actively growing defects it is necessary to be able to assess the
 pipeline system failure operation probability during a certain period,
 taking into account its actual level of defectiveness. The pipeline limit
 state comes when the burst pressure, considered as a random variable,
 reaches an unacceptable level, or when the defect depth, also a random
 variable, exceeds the predetermined limit value.

 That is why in the method they consider two possible failures for a single
 pipeline cross section with the on-surface and longitudinally oriented
 defect of \emph{metal-loss} type:

 \describe{
    \item{\emph{rupture}}{a decrease of value of failure pressure down
    to the operating pressure.}
    \item{\emph{leak}}{increase of corrosion depth (defect) up to the
    specified ultimate permissible fraction of pipe wall thickness.}
 }

Since up to now no methods existed which would give absolutely correct POF
assessments they suggest simple fiddling with random values of affecting
factors without deeping into intrinsic mechanisms of corrossion. For this
purpose they choose classical
\href{https://en.wikipedia.org/wiki/Monte_Carlo_method#Monte_Carlo_and_random_numbers}{Monte-Carlo simulation}
within the \href{https://en.wikipedia.org/wiki/Principle_of_maximum_entropy}{Principle of maximum entropy}.
The latter allows to avoid doubtful and excessive preferences and
detalization when choosing probability distribution models for failure
factors and for \emph{inline inspection} measurements.
}
\details{
Since for all influence factors they can more or less assume range limits,
 the \emph{uniform distribution} gets the maximum entropy in this context
 (see \href{https://www.bipm.org/documents/20126/2071204/JCGM_101_2008_E.pdf/325dcaad-c15a-407c-1105-8b7f322d651c}{JCGM 101:2008}).
 That is why parameters of corrosion defects measured during the
 \emph{inline inspection} as well as regime parameters and engineering
 characteristics of pipe segment - all they are simulated by
 \code{\link{runif}}.

 \code{\link{runif}}-limits for depth of corrosion defect are associated
 with precision of commonly applied measurement instruments. For traditionally
 exploited ultrasonic control those limits are well-known and can reach up to
 10 \% of pipe wall thickness. Whereas uncertainty of defect longitudinal
 length may be more than enough constrained with 5 \%.

 Recommendations for choosing stochastic characteristics of pipe
 engineering factors (i.e. crossection diameter, wall thickness and material
 strength) are taken from aggregated review of \emph{Timashev et al.} but
 gently transformed for compatibility with
 \href{https://en.wikipedia.org/wiki/Principle_of_maximum_entropy}{Principle of maximum entropy},
 i.e. \code{\link{runif}}.

 Uncertainties of regime parameters in stohastic models are set minimized
 by regarding only precision of metering devices which commonly applied in
 district heating networks. For temperature it is about 2 °C.

 Since the rate of corrosion processes in the pipe wall is a consequence of
 physical and chemical processes occurring at the atomic scale,
 it depends on a large number of environmental factors differently and
 ambiguously. That is why various deterministic and stochastic models can
 be potentially involved in POF assessment. For that purpose radial and
 longitudinal corrosion rate can be independently formulated as random value
 generation functions. They only admit that change in depth and length of
 corrosion defects in time is close to linear for the generated value of
 corrosion rate.
}
\examples{
 library(pipenostics)

\donttest{
# Let's consider a pipe in district heating network with
diameter           <- 762         # [mm]
wall_thickness     <-  10         # [mm]
UTS                <- 434.3697    # [MPa]

# which transfers heat-carrier (water) at
operating_pressure <-   0.588399  # [MPa].
temperature        <-  95         # [°C]

# During inline inspection four corroded areas (defects) are detected with:
depth  <- c(2.45,  7.86,   7.93,   8.15)  # [mm]

# whereas the length of all defects is not greater 200 mm:
length <- rep(200, 4)  # [mm]

# Corrosion rates in radial and in longitudinal directions are not well-known and
# may vary in range .01 - .30 mm/year:
rar = function(n) stats::runif(n, .01, .30) / 365
ral = function(n) stats::runif(n, .01, .30) / 365

# Then POFs related to each corroded area are near:
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv")
print(pof)
# 0.000000 0.252510 0.368275 0.771595

# So, the POF of pipe is near
print(max(pof))
# 0.771595

# The value of POF changes in time. So, in a year after inline inspection of
# the pipe we can get something near
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv", days = 365)
print(pof)
# 0.000000 0.525539 0.648359 0.929099

# for entire pipe we get something near:
print(max(pof))
# 0.929099

# Two years ago before inline inspection the pipe state was rather good:
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv", days = -2 * 365)

print(pof)
# 0.000000 0.040780 0.072923 0.271751

# for entire pipe we get something near:
print(max(pof))
# 0.271751


}
}
\references{
\enumerate{
 \item  S. Timashev and A. Bushinskaya, \emph{Diagnostics and Reliability
   of Pipeline Systems}, Topics in Safety, Risk, Reliability and Quality 30,
   \strong{DOI 10.1007/978-3-319-25307-7}.

 \item \href{https://www.bipm.org/en/home}{BIPM}. Guides in Metrology (GUM).
   \href{https://www.bipm.org/documents/20126/2071204/JCGM_101_2008_E.pdf/325dcaad-c15a-407c-1105-8b7f322d651c}{JCGM 101:2008}.
   Evaluation of measurement data – \strong{Supplement 1} to the \emph{Guide to
   the expression of uncertainty in measurement} –
   Propagation of distributions using a \emph{Monte Carlo} method.
 }
}
\concept{Failure probability}
