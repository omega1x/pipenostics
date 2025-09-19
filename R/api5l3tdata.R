#' API 5L. Values of SMYS and UTS
#'
#' Data represents specified minimum yield strength (SMYS) and ultimate
#' tensile strength (UTS) both achieved when producing line pipes
#' according to regulatory specifications.
#'
#' @format A data frame with 57 rows and 4 variables:
#'
#'\describe{
#'  \item{grade}{
#'    designation of standard grade of manufactured pipe.
#'    Type: \code{\link[checkmate]{assert_character}}.
#'  }
#'  \item{smys}{\emph{SMYS} - specified minimum yield strength, [\emph{psi}].
#'    Type: \code{\link[checkmate]{assert_double}}.
#'  }
#'  \item{uts}{
#'    \emph{UTS} - ultimate tensile strength, [\emph{psi}].
#'    Type: \code{\link[checkmate]{assert_double}}.
#'  }
#'  \item{origin}{
#'    Identifier for the information origin regarding the specifications of
#'    pipe, []:
#'    \describe{
#'      \item{\code{10}}{
#'        \href{https://law.resource.org/pub/us/cfr/ibr/002/api.5l.2004.pdf}{
#'         API SPECIFICATION 5L
#'        }.Table 3A
#'      }
#'      \item{\code{3}}{
#'        \href{https://docs.cntd.ru/document/1200002056}{GOST 20295-85}.
#'        Table 7
#'      }
#'      \item{
#'        \code{11}}{\href{https://docs.cntd.ru/document/1200103221}{
#'           GOST 31443-2012
#'         }. Tables 6, 7
#'      }
#'    }
#'    Type: \code{\link[checkmate]{assert_integer}}.
#'  }
#'}
#' @references
#'  \enumerate{
#'    \item \href{https://law.resource.org/pub/us/cfr/ibr/002/api.5l.2004.pdf}{
#'          API SPECIFICATION 5L}. Specification for Line Pipe
#'    \item \href{https://docs.cntd.ru/document/1200002056}{GOST 20295-85}.
#'          Steel welded pipes for main gas-and-oil pipelines. Specifications
#'    \item \href{https://docs.cntd.ru/document/1200103221}{GOST 31443-2012}.
#'          Steel pipes for crafts pipelines. Specifications.
#'  }
#' @examples
#' library(pipenostics)
#'
#' head(api5l3tdata)
#'
"api5l3tdata"
