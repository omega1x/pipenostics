#' Minenergo-278. Thermal conductivity terms of pipe insulation materials
#'
#' Data represent values of terms (intercept and factor) for calculating thermal
#' conductivity of pipe insulation as a linear function of temperature of
#' heat carrier (water).
#' Those values are set for different insulation materials in
#' Appendix 5.3 of \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278}
#' as norms.
#'
#' Usually the data is not used directly. Instead use function \code{\link{m278inshcm}}.
#'
#' @family Minenergo
#' @format A data frame with 39 rows and 4 variables:
#' \describe{
#'   \item{id}{Number of insulation material table 5.1 of Appendix 5.3 in
#'    \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278}.
#'    Type: \code{\link{assert_integerish}}.
#'    }
#'   \item{material}{Designation of insulation material more or less similar
#'   to those in table 5.1 of Appendix 5.3 in
#'   \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278}.
#'   Type: \code{\link{assert_character}}.
#'   }
#'   \item{lambda}{Value for intercept, [\emph{mW/m/°C}].
#'   Type: \code{\link{assert_integer}}.
#'   }
#'   \item{k}{Value for factor. Type: \code{\link{assert_integer}}.}
#'  }
#' @source \url{http://www.complexdoc.ru/ntdtext/547103/}
"m278insdata"