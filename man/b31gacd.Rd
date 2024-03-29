% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/b31gacd.R
\name{b31gacd}
\alias{b31gacd}
\title{ASME B31G. Allowable corrosion depth in pipe}
\usage{
b31gacd(dep, maop, d, wth, l)
}
\arguments{
\item{dep}{design pressure of pipe, [\emph{PSI}]. Type: \code{\link{assert_double}}.}

\item{maop}{maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}]. Type: \code{\link{assert_double}}.}

\item{d}{nominal outside diameter of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.}

\item{wth}{nominal wall thickness of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.}

\item{l}{measured maximum longitudinal length of corroded area, [\emph{inch}].
Type: \code{\link{assert_double}}.}
}
\value{
allowable depth of the corroded area in the pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.
}
\description{
Calculate allowable depth of the corroded area in the pipe.
}
\examples{
 library(pipenostics)

 b31gacd(1093, 910, 30, .438, 7.5)
 # [1] 0.249  # [inch]

}
\references{
\href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}.
 Manual for determining the remaining strength of corroded pipelines. A
 supplement to \emph{ASTME B31} code for pressure piping.
}
\seealso{
Other ASME B31G functions: 
\code{\link{b31crvl}()},
\code{\link{b31gacl}()},
\code{\link{b31gafr}()},
\code{\link{b31gdep}()},
\code{\link{b31gmodpf}()},
\code{\link{b31gops}()},
\code{\link{b31gpf}()},
\code{\link{b31gsap}()}
}
\concept{ASME B31G functions}
