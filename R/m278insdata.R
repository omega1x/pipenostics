#' Minenergo-278. Heat conductivity factors of pipe insulation materials
#'
#' Data represents values for calculating heat conductivity factor of pipe
#' insulation as a linear function of temperature of water as a heat carrier.
#' Those values are set for different insulation materials in
#' Appendix 5.3 of \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278}
#' as norms.
#'
#' Usually the data is not used directly. Instead use function \code{m278ins}.
#'
#' @family Minenergo
#'
#' @format A data frame with 39 rows and 4 variables:
#' \describe{
#'   \item{id}{Identifier of insulation material according to table 5.1 of
#'     Appendix 5.3 in \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278}.
#'   }
#'   \item{material}{Designation of insulation material more or less similar
#'   to those in table 5.1 of Appendix 5.3 in
#'   \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278}.}
#'   \item{lambda}{Value for intercept, [mW/m/°C ]}
#'   \item{k}{Value for factor}
#'  }
#' @source \url{http://www.complexdoc.ru/ntdtext/547103/}
"m278insdata"
