#' Minenergo-325. Normative heat losses of pipe
#'
#' Data represents values of heat losses officially accepted by
#' \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325} as
#' norms. Those values represent heat flux that is legally
#' affirmed to be emitted per meter during an hour by steel pipe of district
#' heating system with water as a heat carrier.
#'
#' Data is organized as a full factorial design, whereas for some factorial
#' combinations \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325}
#' does not provide values. For that cases values are postulated by practical
#' reasons in Siberian cities and marked with source label \emph{sgc}.
#'
#' Usually the data is not used directly. Instead use function \code{\link{m325nhl}}.
#'
#' @family Minenergo
#'
#' @format A data frame with 17328 rows and 8 variables:
#' \describe{
#'   \item{source}{Identifier of data source: identifiers suited with
#'     glob \emph{t?p?} mean appropriate \emph{table ?.?} in
#'     \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325};
#'     identifier \emph{sgc} means that values are additionally
#'     postulated (see \emph{Details}).
#'   }
#'   \item{epoch}{Year depicting the epoch when the pipe is put in operation after laying or total overhaul.}
#'   \item{laying}{Type of pipe laying depicting the position of pipe in space. Only five types of
#'   pipe laying are considered.}
#'   \item{exp5k}{Logical indicator for pipe regime: if \code{TRUE} pipe is operated more that 5000 hours per year.}
#'   \item{insulation}{Identifier of insulation that covers the exterior of pipe:
#'   \code{0} - no insulation; \code{1} - foamed polyurethane or analogue; \code{2} - polymer concrete.}
#'   \item{diameter}{Nominal internal diameter of pipe, [mm].}
#'   \item{temperature}{Operational temperature of pipe, [°C].}
#'   \item{flux}{Heat flux emitted by every meter of pipe during an hour, [kcal/m/hour].}
#'  }
#' @source \url{http://docs.cntd.ru/document/902148459}
"m325nhldata"
