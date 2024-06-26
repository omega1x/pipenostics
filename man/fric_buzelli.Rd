% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fric_buzelli.R
\name{fric_buzelli}
\alias{fric_buzelli}
\title{Estimate pipe friction factor with Buzelli formula}
\usage{
fric_buzelli(reynolds, roughness = 0, strict = FALSE)
}
\arguments{
\item{reynolds}{Reynolds number, []. Type: \code{\link{assert_double}}.}

\item{roughness}{relative roughness, []. Type: \code{\link{assert_double}}.}

\item{strict}{calculate only inside the precision region. Type: \code{\link{assert_flag}}.}
}
\value{
pipe friction factor, []. Type: \code{\link{assert_double}}.
}
\description{
Estimate \emph{Darcy friction factor} explicitly with extremely accurate
 \emph{Buzelli} approximation of \emph{Colebrook equation}.
}
\details{
Buzelli's formula is reported to be extremely accurate in the region:
 \itemize{
   \item \code{3.0e3 <= reynolds  <= 3.0e8}
   \item \code{    0 <= roughness <= 0.05}
 }

 In \code{strict = TRUE} mode argument values outside this precision region
 are not allowed, whereas in \code{strict = FALSE} either \emph{NA}s are
 generated in that case or calculation for laminar flow is performed when
 \code{reynolds < 2100.0}.
}
\examples{
 library(pipenostics)

 fric_buzelli(c(2118517, 2000, 2118517), c(1e-6, 70e-3/1, 7e-3/1))
 # [1] 0.01031468 0.03200000 0.03375076  # []

 fric_buzelli(c(2118517, 5500, 2118517), c(1e-6, 50e-3/1, 7e-3/1), TRUE)
 # [1] 0.01031468 0.07556734 0.03375076
}
\references{
\enumerate{
  \item  Offor, U. and Alabi, S. (2016) \emph{An Accurate and Computationally Efficient Explicit Friction Factor Model}.
 Advances in Chemical Engineering and Science, \emph{6}, pp. 237-245. \doi{http://dx.doi.org/10.4236/aces.2016.63024}.

  \item Buzzelli, D. (2008) \emph{Calculating friction in one step}. Machine Design, \emph{80} (12), pp. 54–55.
}
}
\seealso{
Other Fluid properties: 
\code{\link{fric_romeo}()},
\code{\link{fric_vatankhan}()},
\code{\link{re_u}()}
}
\concept{Fluid properties}
