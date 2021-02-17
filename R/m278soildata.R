#' Minenergo-278. Thermal conductivity of subsoil surrounding pipe
#'
#' Data represent normative values of thermal conductivity of subsoils
#' which can surround pipes according to Table 5.3 of Appendix 5.3 in
#' \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278}.
#'
#' @family Minenergo
#'
#' @format A data frame with 15 rows and 3 variables:
#' \describe{
#'   \item{subsoil}{Geological name of subsoil. Type: \code{\link{assert_character}}.}
#'   \item{state}{The degree of water penetration to the subsoil. Type: \code{\link{assert_character}}.}
#'   \item{lambda}{Value of thermal conductivity of subsoil regarding water penetration, [\emph{W/m/°C}].
#'   Type: \code{\link{assert_double}}.
#'   }
#'  }
#' @source \url{http://www.complexdoc.ru/ntdtext/547103/}
"m278soildata"