#' Diagnostics, reliability and predictive maintanence of pipeline systems
#'
#'
#' The motivation for the package was to aggregate to some extent the separate
#' knowledge about reliability, diagnostics and predictive maintanence of
#' pipeline systems suitable for pipe holders. Distributing such knowledge
#' with \emph{R}-package seemed the most attractive option for white-collar
#' engineers, having utilized spreadsheet software as a mainstream.
#'
#' Aiming to avoid portability and accessibility problems made us search ways to
#' restrict source code development by functionality of \emph{base R} only,
#' eluding as long as possible any external packages. The package also appeares
#' to promote the beaten \emph{R} to those who want to survive in the
#' belligerent environment of green snake lovers.
#'
#' @section ASME B31G:
#'   It is recognized by pipeline companies that some sections of high pressure
#'   pipelines particularly those installed a number of years ago, have
#'   experienced some corrosion. Where corrosion is found, pipeline operators have
#'   been deeply concerned about the need for a method of determining the
#'   remaining strength of these corroded areas. If the corrosion does not
#'   penetrate the pipe wall, what is the pressure containing capability of
#'   the remaining pipe metal in terms of its ability to continue to operate
#'   safely at the maximum allowable operating pressure (MAOP) of the pipeline
#'   system?
#'
#'   Thus, one of the needs of the pipeline industry has been a procedure that
#'   will help operators, particularly field personnel, make decisions on existing
#'   pipelines, when exposed for any purpose, as to whether any corroded region
#'   may be left in service or whether it needs to be repaired or replaced.
#'   Such detenninations must be based upon sound research and extensive testing
#'   in order to provide safe and conservative guidelines on which to base field
#'   decisions.
#'
#'   The \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}
#'   provides procedures to assist in this determination.
#'
#'   \emph{Appendix A} to \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}
#'   shows the source code for determining the allowable length and maximum
#'   allowable working pressure. The \emph{b31g*} package functions reproduce the
#'   idea of \emph{CRVL.BAS}. They are natively vectorized, but remain simple
#'   without argument checks. Thus, use only physically conditioned values as input
#'   to prevent erroneous and meaningless output.
#'
#' @docType package
#' @name pipenostics
NULL
