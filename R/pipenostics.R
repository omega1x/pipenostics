#' Engineering, diagnostics, reliability and predictive maintenance of pipeline systems
#'
#'
#' The motivation for the package was to aggregate to some extent the separate
#' knowledge about engineering, reliability, diagnostics and predictive maintenance of
#' pipeline systems suitable for pipe holders. Distributing such knowledge
#' with \emph{R}-package seemed the most attractive option for white-collar
#' engineers, having utilized spreadsheet software as a mainstream.
#'
#' Aiming to avoid portability and accessibility problems made us search ways to
#' restrict source code development by functionality of few external packages.
#'
#' The next values describing technological conditions, material properties of
#' pipe and defect parameters are used as arguments throughout the most
#' functions concerning corrosion diagnostics:
#'
#' \describe{
#'   \item{\emph{maop}}{maximum allowable operating pressure - \href{https://en.wikipedia.org/wiki/Maximum_allowable_operating_pressure)}{MAOP},
#'      [\href{https://en.wikipedia.org/wiki/Pounds_per_square_inch}{PSI}]}
#'   \item{\emph{d}}{nominal outside diameter of the pipe, [\href{https://en.wikipedia.org/wiki/Inch)}{inch}]}
#'   \item{\emph{wth}}{nominal wall thickness of the pipe, [\href{https://en.wikipedia.org/wiki/Inch}{inch}]}
#'   \item{\emph{smys}}{specified minimum yield of stress (\href{https://en.wikipedia.org/wiki/Specified_minimum_yield_strength}{SMYS})
#'     as a characteristics of steel strength, [\href{https://en.wikipedia.org/wiki/Pounds_per_square_inch}{PSI}]}
#'   \item{\emph{depth}}{measured maximum depth of the corroded area, [\href{https://en.wikipedia.org/wiki/Inch}{inch}]}
#'   \item{\emph{l}}{measured maximum longitudial length of the corroded area, [\href{https://en.wikipedia.org/wiki/Inch}{inch}]}
#'}
#' @section ASME B31G-1991:
#'   It is recognized by pipeline companies that some sections of high pressure
#'   pipelines particularly those installed a number of years ago, have
#'   experienced some corrosion. Where corrosion is found, pipeline operators have
#'   been deeply concerned about the need for a method of determining the
#'   remaining strength of these corroded areas. If the corrosion does not
#'   penetrate the pipe wall, what is the pressure containing capability of
#'   the remaining pipe metal in terms of its ability to continue to operate
#'   safely at the maximum allowable operating pressure (\emph{MAOP}) of the
#'   pipeline system?
#'
#'   Thus, one of the needs of the pipeline industry has been a procedure that
#'   will help operators, particularly field personnel, make decisions on existing
#'   pipelines, when exposed for any purpose, as to whether any corroded region
#'   may be left in service or whether it needs to be repaired or replaced.
#'   Such determinations must be based upon sound research and extensive testing
#'   in order to provide safe and conservative guidelines on which to base field
#'   decisions.
#'
#'   The \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}
#'   provides procedures to assist in this determination.
#'
#'   \emph{Appendix A} to \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}
#'   shows the source code for determining the allowable length and maximum
#'   allowable working pressure. The \emph{b31g*} and \code{\link{crvl}}
#'   functions reproduce the idea of \emph{CRVL.BAS}. They are natively vectorized.
#'
#'   Usage of \code{\link{crvl}} function that imitates the output of _CRVL.BAS_
#'   is presentaed in \emph{Example 1}.
#'
#' @examples
#' ## Example 1
#' crvl(maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1, l = 7.5)
#'
#'  # -- Calculated data --
#'  # Intermediate factor (A) = 1.847
#'  # Design pressure = 1093 PSI; Safe pressure = 1093 PSI
#'  # Pipe may be operated safely at MAOP, 910 PSI
#'  # With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847
#'  # With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000
#'
#' @section ASME B31G-2012:
#' An effort was undertaken to update the \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991} up to
#' \href{https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines}{ASME B31G-2012}
#' document to recognize certain other corrosion evaluation methods that have
#' proven sound and that have seen successful use in the pipeline industry.
#' Incorporation of these other methods provides us with a formalized framework
#' within which to use such methodologies.
#'
#' Nevertheless, to preserve simplicity of traditional inline measurements during
#' inspections we only consider \strong{Analysis Level 1}.
#'
#' As noted in \href{https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines}{ASME B31G-2012},
#' \strong{Level 1} evaluation is quite suitable for use in prioritizing corrosion
#' defects identified by inline inspection.
#'
#' @docType package
#' @name pipenostics
NULL
