#' Minenergo-325. Normative heat loss data
#'
#' Data represent values of specific heat loss power through the thermal
#' insulation officially accepted by
#' \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325} as
#' norms. Those values are maximums which are legally
#' affirmed to contribute to normative heat loss \eqn{Q_{NHL}} of
#' district heating systems with water as a heat carrier.
#'
#' Data is organized as a full factorial design, whereas for some factorial
#' combinations
#' \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}
#' does not provide values. For that cases values are postulated by practical
#' reasons in Siberian cities and marked with source label \emph{sgc}.
#'
#' Usually the data is not used directly. Instead use function
#' \code{\link{m325nhl}}.
#'
#' @family Minenergo
#'
#' @format A data frame with 17328 rows and 8 variables:
#' \describe{
#'   \item{source}{Identifier of data source: identifiers suited with
#'     glob \emph{t?p?} mean appropriate \emph{table ?.?} in
#'     \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325};
#'     identifier \emph{sgc} means that values are additionally
#'     postulated (see \emph{Details}).
#'     Type: \code{\link[checkmate]{assert_character}}.
#'   }
#'   \item{epoch}{
#'     Year depicting the epoch when the pipe is put in operation after laying
#'     or total overhaul. Type: \code{\link[checkmate]{assert_integer}}.
#'   }
#'   \item{laying}{
#'     Type of pipe laying depicting the position of pipe in space. Only five
#'     types of pipe laying are considered:
#'     \itemize{
#'      \item \code{air},
#'      \item \code{channel},
#'      \item \code{room},
#'      \item \code{tunnel},
#'      \item \code{underground}.
#'     }
#'     Type: \code{\link[checkmate]{assert_character}}.
#'   }
#'
#'   \item{exp5k}{
#'     Logical indicator for pipe regime: if \code{TRUE} pipe is
#'     operated more that \code{5000} hours per year.
#'     Type: \code{\link[checkmate]{assert_logical}}.
#'   }
#'
#'   \item{insulation}{
#'     Identifier of insulation that covers the exterior of pipe:
#'     \describe{
#'       \item{\code{0}}{no insulation}
#'       \item{\code{1}}{foamed polyurethane or analogue}
#'       \item{\code{2}}{polymer concrete}
#'     }
#'    Type: \code{\link[checkmate]{assert_integerish}}.
#'   }
#'
#'   \item{d}{
#'     Nominal (outside) diameter of pipe, [\emph{mm}].
#'     Type: \code{\link[checkmate]{assert_double}}.
#'   }
#'
#'   \item{temperature}{
#'     Operational temperature of pipe, [\emph{°C}].
#'     Type: \code{\link[checkmate]{assert_double}}.
#'   }
#'
#'   \item{loss}{
#'     Normative value of specific heat loss power equal to heat flux output by
#'     one-meter length steel pipe during an hour,
#'     [\emph{kcal/m/h}].
#'     Type: \code{\link[checkmate]{assert_double}}.}
#'  }
#'
#' @source \url{https://docs.cntd.ru/document/902148459}
#' @examples
#' library(pipenostics)
#'
#' head(m325nhldata)
#'
"m325nhldata"
