#'  Specifications of the manufactured pipes
#'
#' Data represents the nominal specifications of steel pipes produced
#' by the industry according to regulatory standards.
#'
#' @family utils
#'
#' @format A data frame with 6064 rows and 5 variables:
#' \describe{
#'   \item{d}{Nominal outside diameter of the manufactured pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.}
#'   \item{wth}{Nominal wall thickness of the manufactured pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.}
#'   \item{rho}{Nominal mass density of the steel rank applied in the pipe manufacturing, [\emph{g/cm³}]. Type: \code{\link{assert_double}}.}
#'   \item{mass}{Nominal mass of one-meter length pipe segment, [\emph{kg}]. Type: \code{\link{assert_double}}.}
#'   \item{origin}{
#'      Identifier for the information origin regarding the specifications of pipe, []:
#'      \describe{
#'        \item{\code{1}}{\href{https://www.asme.org/codes-standards/find-codes-standards/b36-10m-welded-seamless-wrought-steel-pipe/2018/nondrm-enabled-pdf}{ASME B36.10M-2018}}
#'        \item{\code{2}}{\href{https://www.asme.org/codes-standards/find-codes-standards/b36-19m-stainless-steel-pipe}{ASME B36.19M-2018}}
#'        \item{\code{3}}{\href{https://docs.cntd.ru/document/1200002056}{GOST 20295-85}. Table 1}
#'        \item{\code{4}}{\href{https://docs.cntd.ru/document/1200129487}{GOST 33229-2015}. Table 1}
#'        \item{\code{5}}{\emph{GOST 33229-2015}. Table 2}
#'        \item{\code{6}}{\href{https://docs.cntd.ru/document/1200144603}{GOST R 57423-2017}. Table 1}
#'        \item{\code{7}}{\emph{GOST R 57423-2017}. Table 2}
#'        \item{\code{8}}{\emph{GOST R 57423-2017}. Table 3}
#'        \item{\code{9}}{\emph{GOST R 57423-2017}. Table 4}
#'      }
#'      Type: \code{\link{assert_integer}}.
#'   }
#' }
#' \emph{NOTE!} Due to numerous typos in origins with identifiers \code{4}, \code{5} all mass values in those origins are the recalculations made with formula
#'   \deqn{M = 0.02466 \cdot w\left(d - w \right )}
#' where
#' \itemize{
#'    \item \eqn{M} - mass of one-meter pipe segment, [\emph{kg}]
#'    \item \eqn{d} - nominal outside diameter of the manufactured pipe, [\emph{mm}]
#'    \item \eqn{w} (\code{wth}) - nominal wall thickness of the manufactured pipe, [\emph{mm}]
#'  }
#'
#' @references
#'  \enumerate{
#'    \item \href{https://www.asme.org/codes-standards/find-codes-standards/b36-10m-welded-seamless-wrought-steel-pipe/2018/nondrm-enabled-pdf}{ASME B36.10M-2018}. Welded and seamless wrought steel pipe
#'    \item \href{https://www.asme.org/codes-standards/find-codes-standards/b36-19m-stainless-steel-pipe}{ASME B36.19M-2018}. Stainless steel pipe
#'    \item \href{https://docs.cntd.ru/document/1200002056}{GOST 20295-85}. Steel welded pipes for main gas-and-oil pipelines. Specifications
#'    \item \href{https://docs.cntd.ru/document/1200129487}{GOST 33229-2015}. Tubes for boiler and heat exchanging equipment. Technical specifications. Part 1. Seamless steel pipes to work under pressure not more than 6,4 MPa and at temperatures not exceeding 400°C
#'    \item \href{https://docs.cntd.ru/document/1200144603}{GOST R 57423-2017}. Tubes for boiler and heat exchanging equipment. Part 2. Seamless steel tubes for pressure purposes more 6,4 MPa and temperatures exceeding 400°C. Specifications
#'  }
"b36pipedata"
