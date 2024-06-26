% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reynolds.R
\name{re_u}
\alias{re_u}
\alias{re_v}
\alias{re_m}
\title{Estimate Reynolds number}
\usage{
re_u(d, mu, u, rho)

re_v(d, mu, v, rho)

re_m(d, mu, m)
}
\arguments{
\item{d}{internal diameter of pipe, [\emph{m}]. Type: \code{\link{assert_double}}.}

\item{mu}{dynamic viscosity of fluid in pipe, [\emph{kg/m/s}]. Type: \code{\link{assert_double}}.}

\item{u}{mean velocity of fluid in pipe, [\emph{m/s}]. Type: \code{\link{assert_double}}.}

\item{rho}{mass density of fluid in pipe, [\emph{kg/m^3}]. Type: \code{\link{assert_double}}.}

\item{v}{volumetric flow rate of fluid in pipe, [\emph{m^3/s}]. Type: \code{\link{assert_double}}.}

\item{m}{mass flow rate of fluid in pipe, [\emph{kg/s}]. Type: \code{\link{assert_double}}.}
}
\value{
\emph{Reynolds number} - a dimensionless quantity that reveals the ratio
 between inertial and viscous forces in the fluid, []. Type: \code{\link{assert_double}}.
}
\description{
Estimate \emph{Reynolds number} for fluid flow in a cylindrical pipe.
}
\details{
The calculation of \emph{Reynolds number} is bounded by physically
 reasonable limits of fluid properties found in domain specificity of
 the package.
}
\examples{
 library(pipenostics)

 # Reynolds numbers for typical district heating water flows at temperature
 # near 25 C in a set of pipes with different sizes:
 range(re_u(seq(.25, 1, 0.05), .89, 1, 1000))
 # [1]  280.8989 1123.5955

}
\seealso{
Other Fluid properties: 
\code{\link{fric_buzelli}()},
\code{\link{fric_romeo}()},
\code{\link{fric_vatankhan}()}
}
\concept{Fluid properties}
