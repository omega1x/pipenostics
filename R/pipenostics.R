#' Diagnostics, reliability and predictive maintenance of pipeline systems
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
#' Since most functions have native argument vectorization usage of those
#' functions with fast
#' \href{https://cran.r-project.org/package=data.table}{data.table}
#' framework is strongly encourage when processing large data sets.
#'
#' For that purpose arguments for all package functions are thoroughly checked
#' for type consistency and physical sense using asserts and tests from
#' \code{\link{checkmate}} package. Moreover, in package documentation we
#' borrow type designations according to \code{\link{checkmate}} notation.
#'
#'
#' @section 1. Corrosion diagnostics:
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
#'
#' @section 1.1 ASME B31G-1991:
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
#'   allowable working pressure. The \emph{b31g*} and \code{\link{b31crvl}}
#'   functions reproduce the idea of \emph{CRVL.BAS}. They are natively vectorized.
#'
#'   Usage of \code{\link{b31crvl}} function that imitates the output of _CRVL.BAS_
#'   is presentaed in \emph{Example 1}.
#'
#' @examples
#' ## Example 1
#' b31crvl(maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1, l = 7.5)
#'
#'  # -- Calculated data --
#'  # Intermediate factor (A) = 1.847
#'  # Design pressure = 1093 PSI; Safe pressure = 1093 PSI
#'  # Pipe may be operated safely at MAOP, 910 PSI
#'  # With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847
#'  # With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000
#'
#' @section 1.2 ASME B31G-2012:
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
#' @section 1.3 Other models:
#' Other approaches for operating with corrosion data are mostly aimed on
#' failure pressure calculations. Models like \code{\link{dnvpf}},
#' \code{\link{shell92pf}}, and \code{\link{pcorrcpf}} assume different shapes
#' of corrosion defects and usage conditions for some cases. So, it is
#' encouraged first to find out which model is most suitable for solving
#' some real world problem.
#'
#' @section 2. Heat losses:
#'
#' Heat loss is the energy characteristic of district heating networks. It is
#' the amount of heat energy spent on the transportation and distribution of
#' heat energy from the source to the consumers.
#'
#' Heat losses depend on the operating temperature, technical condition,
#' volume and configuration of the district heating network, as well as on
#' climatic factors. Heat losses are additive being the sum of the heat losses
#' of individual pipeline segments.
#'
#' Determination of heat losses for pipeline segments hereinafter is called
#' \emph{heat loss localization}.
#'
#' It is assumed that actual heat loss (\strong{AHL}) of pipeline segment has two
#' contributions: normative heat loss (\strong{NHL}) and extra-normative heat loss
#' (\strong{ExNHL}).So we can write:
#'
#' \deqn{
#'  AHL = NHL + ExNHL, ExNHL > 0.
#' }
#'
#' Localization of \strong{ExNHL} is an important part of health maintenance
#' activities of district heating network operation. One can determine
#' \strong{ExNHL} of pipeline segment as a positive difference between
#' \strong{AHL} and \strong{NHL} and it is the most natural way. For that
#' purpose \href{http://docs.cntd.ru/document/902148459}{Minenergo-325} and
#' \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo-278} methods
#' for postulating \strong{NHL} are considered.
#' \href{http://docs.cntd.ru/document/902148459}{Minenergo-325} lists
#' legally affirmed maximum values of heat flux that is allowed to be emitted by
#' steel pipes (see \code{\link{m325nhl}}). Higher emission is treated as
#' \strong{ExNHL}. \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo-278}
#' gives method for engineering calculation of \strong{NHL} considering technical
#' condition of pipeline segment (see \code{\link{m278hlcha}},
#' \code{\link{m278hlund}}, and \code{\link{m278hlair}}).
#'
#'
#' @docType package
#' @name pipenostics
NULL
