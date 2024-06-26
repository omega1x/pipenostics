#' Minenergo-325. Test bench of district heating network
#'
#' Data describes a virtual test bench of branched district heating network
#' by exposing parameters associated with
#' \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#' They treat data as a snapshot of network state and use it
#' primarily for static thermal-hydraulic computations and topology effects.
#'
#' The test bench has the next configuration:
#'
#' \figure{m325nxdata.png}
#'
#' As it may be seen from the figure there is a particularity in topology of
#' the provided directed graph: each node has only single ancestor. Hence one
#' of isomorphic representation of such directed graph is a
#' \code{\link{data.frame}} in which each row describes a node along with its
#' incoming edge and each column contains an attribute value for that node or
#' an attribute value for its incoming edge.
#'
#' Since they deal with incoming edges and hence nodes are all flow acceptors
#' the natural enumeration of nodes is by acceptor id.
#'
#' Note that to leverage
#' \href{https://cran.r-project.org/package=igraph}{igraph}
#' functionality for plotting there is a zero sender of flow.
#'
#' @family Minenergo
#'
#' @format A data frame with 22 rows (number of nodes and incoming edges) and 15 variables:
#' \describe{
#'   \item{sender}{
#'     An identifier of node which heat carrier flows out.
#'     Type: any type that can be painlessly coerced to character
#'     by \code{\link{as.character}}.
#'   }
#'
#'   \item{acceptor}{
#'     An identifier of node which heat carrier flows in. According to topology
#'     of test bench considered this identifier should be unique for every row.
#'     Type: any type that can be painlessly coerced to
#'     character by \code{\link{as.character}}.
#'   }
#'
#'   \item{temperature}{
#'     Snapshot of thermal-hydraulic regime state: temperature of heat carrier
#'     (water) sensor-measured on terminal acceptor node, [\emph{°C}].
#'     Type: \code{\link{assert_double}}. \code{NA}s are introduced for nodes
#'     without temperature sensor.
#'   }
#'
#'   \item{pressure}{
#'     Snapshot of thermal-hydraulic regime state:
#'     sensor-measured
#'     \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'     of heat carrier (water) inside the pipe (i.e. acceptor's incoming edge),
#'     [\emph{MPa}].
#'     Type: \code{\link{assert_double}}. \code{NA}s are introduced for nodes
#'     without pressure sensor.
#'   }
#'
#'   \item{flow_rate}{
#'     Snapshot of thermal-hydraulic regime state:
#'     sensor-measured amount of heat carrier (water) on terminal node that is
#'     transferred by pipe (i.e. acceptor's incoming edge) during a period,
#'     [\emph{ton/hour}]. Type: \code{\link{assert_double}}.
#'     \code{NA}s are introduced for nodes without flow rate sensor.
#'   }
#'
#'  \item{d}{
#'    internal diameter of pipe (i.e.diameter of acceptor's incoming edge),
#'    [\emph{m}].
#'    Type: \code{\link{assert_double}}.
#'  }
#'
#'  \item{len}{
#'    pipe length (i.e. length of acceptor's incoming edge), [\emph{m}].
#'    Type: \code{\link{assert_double}}.
#'  }
#'
#'  \item{year}{
#'    year when the pipe (i.e. acceptor's incoming edge) is put in operation after
#'    laying or total overhaul.
#'    Type: \code{\link{assert_integerish}}.
#'  }
#'
#'  \item{insulation}{
#'     identifier of insulation that covers the exterior of pipe (i.e. acceptor's
#'     incoming edge):
#'     \describe{
#'       \item{\code{0}}{no insulation}
#'       \item{\code{1}}{foamed polyurethane or analogue}
#'       \item{\code{2}}{polymer concrete}
#'     }
#'    Type: \code{\link{assert_integerish}}.
#'  }
#'
#'  \item{laying}{
#'     type of pipe laying depicting the position of pipe in space. Only five
#'     types of pipe laying are considered:
#'     \itemize{
#'       \item \code{air},
#'       \item \code{channel},
#'       \item \code{room},
#'       \item \code{tunnel},
#'       \item \code{underground}.
#'     }
#'     Type: \code{\link{assert_character}}.
#'   }
#'
#'  \item{beta}{
#'    logical indicator: should they consider additional heat loss of fittings
#'    located on this pipe (i.e. acceptor's incoming edge)?
#'    Type: \code{\link{assert_logical}}.
#'  }
#'
#'  \item{exp5k}{
#'     logical indicator for regime of pipe (i.e. acceptor's incoming edge): if
#'     \code{TRUE} pipe is operated more that \code{5000} hours per year.
#'     Type: \code{\link{assert_logical}}.
#'   }
#'
#'  \item{roughness}{
#'    roughness of internal wall of pipe (i.e. acceptor's incoming edge),
#'    [\emph{m}].
#'    Type: \code{\link{assert_double}}.
#'  }
#'
#'  \item{inlet}{
#'     elevation of pipe inlet, [\emph{m}].
#'     Type: \code{\link{assert_double}}.
#'  }
#'
#'  \item{outlet}{
#'     elevation of pipe outlet, [\emph{m}].
#'     Type: \code{\link{assert_double}}.
#'  }
#'}
#'
#' @examples
#'  library(pipenostics)
#'
#' # Do not hesitate to use `data.table` and `igraph` for larger chunks of network.
#'
#' # Check for declared topology isomorphism:
#' stopifnot(
#'   all(!duplicated(m325nxdata$acceptor))
#' )
#'
#' # Do all terminal nodes have sensor-measured regime parameters?:
#' terminal_nodes <- subset(m325nxdata, !(acceptor %in% sender))
#' stopifnot(
#'   all(!is.na(subset(terminal_nodes, select = c(temperature, pressure, flow_rate))))
#' )
#'
"m325nxdata"
