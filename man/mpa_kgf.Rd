% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mpa_kgf.R
\name{mpa_kgf}
\alias{mpa_kgf}
\title{Kilogram-force per square cm to megapascals}
\usage{
mpa_kgf(x)
}
\arguments{
\item{x}{pressure (stress) measured in \emph{kilogram-force per square cm},
[\emph{kgf/cm^2}]. Type: \code{\link{assert_double}}.}
}
\value{
pressure (stress) in \emph{megapascals}, [\emph{MPa}].
 Type: \code{\link{assert_double}}.
}
\description{
Convert pressure (stress) measured in \href{https://en.wikipedia.org/wiki/Kilogram-force_per_square_centimetre}{kilogram-force per square cm} (\eqn{kgf/cm^2})
 to \href{https://en.wikipedia.org/wiki/Pascal_(unit)}{megapascals} (MPa)
}
\examples{
 library(pipenostics)

 mpa_kgf(c(10.1971619998, 1))
 # [1] 1.0000000 0.0980665  # [MPa]

}
\seealso{
\code{\link{kgf_mpa}} for converting \emph{megapascals} to \emph{kilogram-force per square cm}

Other units: 
\code{\link{c_k}()},
\code{\link{f_k}()},
\code{\link{inch_mm}()},
\code{\link{k_c}()},
\code{\link{kgf_mpa}()},
\code{\link{loss_flux}()},
\code{\link{mm_inch}()},
\code{\link{mpa_psi}()},
\code{\link{psi_mpa}()}
}
\concept{units}
