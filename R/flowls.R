#' @title
#'  List all possible flow paths in district heating network
#'
#' @description
#'  Find and list all possible paths of heat carrier flow (water) in the given
#'  topology of district heating system.
#'
#' @details
#' Only branched topology without cycles is considered where no more than one
#' incoming edge exists for every \code{acceptor} node. For instance,
#' \code{\link{m325nxdata}} has permitted topology.
#'
#' Though input arguments are natively vectorized their individual values
#' all relate to common part of district heating network, i.e. associated with
#' common object. It is due to isomorphism between vector representation and
#' directed graph of this network. For more details of isomorphic topology
#' description see \code{\link{m325nxdata}}.
#'
#' @param sender
#'    identifier of the node which heat carrier flows out.
#'    Type: any type that can be painlessly coerced to character by
#'    \code{\link{as.character}}.
#'
#' @param acceptor
#'    identifier of the node which heat carrier flows in. According to topology
#'    of test bench considered this identifier should be unique.
#'    Type: any type that can be painlessly coerced to character by
#'    \code{\link{as.character}}.
#'
#' @param use_cluster
#'    utilize functionality of parallel processing on multi-core CPU.
#'    Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @return
#'  Named \code{\link{list}} that contains integer vectors as its elements. The
#'  name of each element in the \code{\link{list}} is the name of
#'  \code{acceptor} associated with terminal node of district heating network.
#'  Each vector in the \code{\link{list}} represents an ordered sequence of
#'  indexes in \code{acceptor} that enumerates incoming edges from starting node
#'  to terminal one. The length of returned \code{link{list}} is equal to number
#'  of terminal nodes for topology considered.
#'  Type: \code{\link[checkmate]{assert_list}}.
#'
#' @seealso
#'  \code{\link{m325nxdata}} for example of topology of district heating
#'  system
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' # Find path from A to B in trivial line topology:
#' flowls("A", "B")
#'
#' # More complex example with two terminal nodes D and E:
#' flowls(c("A", "B", "B"), c("B", "D", "E"))
#'
#' # All possible flow paths in test bench illustrated in `?m325nxdata`:
#' all_paths <- list(
#'   c(12, 13, 11, 8, 4, 1),  # hereinafter indexes of acceptor nodes
#'   c(12, 13, 11, 8, 4, 2),
#'   c(12, 13, 11, 8, 6, 5,  3),
#'   c(12, 13, 11, 8, 6, 7),
#'   c(12, 13, 11, 8, 6, 9),
#'   c(12, 13, 11, 10),
#'   c(12, 13, 14, 15),
#'   c(12, 13, 16, 17),
#'   c(12, 13, 16, 18, 20, 19),
#'   c(12, 13, 16, 18, 20, 21),
#'   c(12, 13, 16, 18, 22, 24),
#'   c(12, 13, 16, 18, 22, 25),
#'   c(12, 13, 16, 18, 20, 23, 26)
#' )
#'
#' # find those paths:
#' with(m325nxdata, flowls(sender, acceptor))
#'
flowls <- function(sender = "A", acceptor = "B", use_cluster = FALSE) {
  # Validate function input ----
  checkmate::assert_vector(
    acceptor,
    any.missing = FALSE, min.len = 1, unique = TRUE
  )
  n <- length(acceptor)
  checkmate::assert_vector(sender, any.missing = FALSE, len = n)
  checkmate::assert_flag(use_cluster)

  acceptor <- as.character(acceptor)
  sender   <- as.character(sender)

  # Validate topology ----
  starting_node_idx <- which(!(sender %in% acceptor))
  checkmate::assert_count(starting_node_idx, positive = TRUE)
  terminal_node_idx <- which(!(acceptor %in% sender))
  checkmate::assert_integer(terminal_node_idx, min.len = 1L)

  # Search algorithm (worker) ----
  worker <- function(path_id) {
    path <- integer(n)  # get *n* from parent env
    segment_counter <- 1L

    # get *terminal_node_idx* from parent env:
    idx <- terminal_node_idx[[path_id]]
    path[[segment_counter]] <- idx

    # get *starting_node_idx* from parent env:
    while (idx != starting_node_idx) {
      segment_counter <- segment_counter + 1L
      # get *acceptor* and *sender* from parent env
      idx <- which(acceptor == sender[acceptor == acceptor[[idx]]])
      path[[segment_counter]] <- idx
    }
    rev(path[path > 0L])
  }

  if (use_cluster) {
    cluster <- parallel::makeCluster(parallel::detectCores() - 1)
    parallel::clusterExport(
      cluster,
      c("n", "terminal_node_idx", "starting_node_idx", "acceptor", "sender"),
      envir = environment()
    )
    stream <- parallel::parLapply(
      cluster,
      structure(
        seq_along(terminal_node_idx), names = acceptor[terminal_node_idx]
      ),
      worker
    )
    parallel::stopCluster(cluster)
    stream
  } else {
    lapply(
      structure(
        seq_along(terminal_node_idx), names = acceptor[terminal_node_idx]
      ),
      worker
    )
  }
}
