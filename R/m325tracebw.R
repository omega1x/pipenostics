#' @title
#'  Minenergo-325. Trace backwards thermal-hydraulic regime for district heating
#'  network
#'
#' @family Regime tracing
#'
#' @description
#'  Trace values of thermal-hydraulic regime (temperature, pressure,
#'  consumption) in the bunched pipeline against the flow direction using norms of
#'  heat flux values prescribed by
#'  \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' @details
#'  The calculated (values of) regime may be considered as representation of
#'  district heating process in conditions of hypothetically perfect
#'  technical state of pipe walls and insulation.
#'
#'  They consider the topology of district heating network much similar to
#'  \code{\link{m325testbench}}:
#'
#'  \figure{m325tracebw.png}
#'
#'  Tracing starts from sensor-equipped nodes and goes backwards, i.e against
#'  the flow direction.
#'
#'  Though some input arguments are natively vectorized their individual values
#'  all relate to common part of district heating network, i.e. associated with
#'  common object. It is due to isomorphism between vector representation and
#'  directed graph of this network. For more details of isomorphic topology
#'  description see \code{\link{m325testbench}}.
#'
#'  Before tracing starts for the next node, previously calculated values of
#'  temperature or pressure at the node are aggregated by either averaging or
#'  by median.
#'  The latter seems more robust for avoiding strong influence of possible
#'  outliers which may come from actual heating transfer anomalies, erroneous
#'  sensor readings or wrong pipeline specifications.
#'
#'  Aggregation for values of consumption at the node is always \code{\link{sum}}.
#'
#' @param sender
#'    identifier of the node which heat carrier flows out.
#'    Type: any type that can be painlessly coerced to character by
#'    \code{\link{as.character}}.
#'
#' @param acceptor
#'    identifier of the node which heat carrier flows in. According to topology
#'    of test bench considered this identifier should be unique for every row.
#'    Type: any type that can be painlessly coerced to character by
#'    \code{\link{as.character}}.
#'
#' @param temperature
#'    \emph{snapshot of thermal-hydraulic regime state}: temperature of heat carrier
#'    (water) sensor-measured on the terminal acceptor node, [\emph{°C}].
#'    Use \code{NA_float_}s for nodes without temperature sensor.
#'    Type: \code{\link{assert_double}}.
#'
#' @param pressure
#'    \emph{snapshot of thermal-hydraulic regime state}: sensor-measured
#'    \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'    of heat carrier (water) inside the pipe (i.e. acceptor's incoming edge),
#'    [\emph{MPa}]. Type: \code{\link{assert_double}}.
#     Use \code{NA_float_}s for nodes without pressure sensor.
#'
#' @param consumption
#'    \emph{snapshot of thermal-hydraulic regime state}:
#'    sensor-measured amount of heat carrier (water) on terminal node that is
#'    transferred by pipe (i.e. acceptor's incoming edge) during a period,
#'    [\emph{ton/hour}]. Type: \code{\link{assert_double}}.
#'    Use \code{NA_float_}s for nodes without consumption sensor.
#'
#' @param d
#'    internal diameter of pipe (i.e.diameter of acceptor's incoming edge),
#'    [\emph{mm}].
#'    Type: \code{\link{assert_double}}.
#'
#' @param len
#'    pipe length (i.e. length of acceptor's incoming edge), [\emph{m}].
#'    Type: \code{\link{assert_double}}.
#'
#' @param year
#'    year when the pipe (i.e. acceptor's incoming edge) is put in operation
#'    after laying or total overhaul.
#'    Type: \code{\link{assert_integerish}}.
#'
#' @param insulation
#'    identifier of insulation that covers the exterior of pipe (i.e. acceptor's
#'    incoming edge):
#'     \describe{
#'       \item{\code{0}}{no insulation}
#'       \item{\code{1}}{foamed polyurethane or analogue}
#'       \item{\code{2}}{polymer concrete}
#'     }
#'    Type: \code{\link{assert_subset}}.
#'
#' @param laying
#'    type of pipe laying depicting the position of pipe in space. Only five
#'    types of pipe laying are considered:
#'    \itemize{
#'      \item \code{air},
#'      \item \code{channel},
#'      \item \code{room},
#'      \item \code{tunnel},
#'      \item \code{underground}.
#'    }
#'    Type: \code{\link{assert_subset}}.
#'
#' @param beta
#'    logical indicator: should they consider additional heat losses of fittings
#'    located on this pipe (i.e. acceptor's incoming edge)?
#'    Type: \code{\link{assert_logical}}.
#'
#' @param exp5k
#'    logical indicator for regime of pipe (i.e. acceptor's incoming edge): if
#'    \code{TRUE} pipe is operated more that \code{5000} hours per year.
#'    Type: \code{\link{assert_logical}}.
#'
#' @param roughness
#'    roughness of internal wall of pipe (i.e. acceptor's incoming edge),
#'    [\emph{m}]. Type: \code{\link{assert_logical}}.
#'
#' @param inlet
#'     elevation of pipe inlet, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param outlet
#'     elevation of pipe outlet, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param method
#'    method of determining \emph{Darcy friction factor}:
#'    \itemize{
#'      \item \code{romeo}
#'      \item \code{vatankhan}
#'      \item \code{buzelli}
#'    }
#'    Type: \code{\link{assert_choice}}.
#'    For more details see \code{\link{dropp}}.
#'
#' @param opinion
#'    method for aggregating values of regime parameters on each node for the
#'    next tracing step:
#'    \describe{
#'       \item{\code{mean}}{values of parameter are averaged before the next
#'       tracing step}
#'       \item{\code{median}}{median of parameter values are used for the next
#'       tracing step}
#'     }
#'    Type: \code{\link{assert_choice}}.
#'
#' @param verbose
#'    logical indicator: should they watch tracing process on console?
#'    Type: \code{\link{assert_flag}}.
#'
#' @param csv
#'    logical indicator: should they incrementally dump results to \emph{csv}-file
#'    while tracing?
#'    Type: \code{\link{assert_flag}}.
#'
#' @param file
#'    name of \emph{csv}-file which they dump results to.
#'    Type: \code{\link{assert_character}} of length 1 that can be used safely
#'    to create a file and write to it.
#'
#' @return
#'    \code{data.frame} containing results of tracing in
#'    long format
#'    (\href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{narrow format})
#'    mostly like it returned by function \code{\link{m325tracefw}}:
#'    \describe{
#'      \item{\code{node}}{
#'        identifier of the node for which regime parameters is calculated.
#'        Values in this vector are identical to those in argument \code{acceptor}.
#'        Type: \code{\link{assert_character}}.
#'      }
#'
#'     \item{\code{trace}}{
#'       concatenated identifiers of nodes from which regime parameters are
#'       traced for the given node. Identifier \code{sensor} is used when
#'       values of regime parameters for the node are sensor readings.
#'       Type: \code{\link{assert_character}}.
#'     }
#'
#'     \item{\code{backward}}{
#'       identifier of tracing direction. It constantly equals to \code{TRUE}.
#'       Type: \code{\link{assert_logical}}.
#'     }
#'
#'     \item{\code{aggregation}}{
#'       aggregation method associated with values of calculated temperature or
#'       pressure in \code{data.frame}'s row for the node:
#'       \describe{
#'          \item{\code{identity}}{
#'            values (opinions) of temperature or pressure as they are
#'            (no aggregation).
#'          }
#'          \item{\code{span}}{
#'            span of values (opinions) of temperature or pressure for the node
#'          }
#'          \item{\code{median}}{
#'            median of values (opinions) of temperature or pressure for the node
#'          }
#'          \item{\code{mean}}{
#'            avaraged values (opinions) temperature or pressure for the node
#'          }
#'       }
#'       Type: \code{\link{assert_character}}.
#'     }
#'
#'    \item{\code{temperature}}{
#'      \emph{snapshot of thermal-hydraulic regime state}: traced temperature of heat
#'      carrier (water) that is associated with the node, [\emph{°C}]
#'      Type: \code{\link{assert_double}}.
#'    }
#'
#'    \item{\code{pressure}}{
#'      \emph{snapshot of thermal-hydraulic regime state}: traced pressure of heat
#'      carrier (water) that is associated with the node, [\emph{MPa}]
#'      Type: \code{\link{assert_double}}.
#'    }
#'
#'    \item{\code{consumption}}{
#'      \emph{snapshot of thermal-hydraulic regime state}: traced pressure of heat
#'      carrier (water) that is associated with the node, [\emph{ton/hour}]
#'      Type: \code{\link{assert_double}}.
#'    }
#'
#'    \item{\code{job}}{
#'      value of trace step counter.
#'      Type: \code{\link{assert_integer}}.
#'    }
#'  }
#'
#' @examples
#' # It is possible to run without specification of argument values:
#' m325tracebw()
#'
#' # Get isomorphic representation of district heating network graph:
#' nx <- pipenostics::m325testbench
#' nx$d <- 1e3*nx$d  # convert [m] to [mm]
#'
#' # When tracing large network graphs put screen log to file
#' output <- do.call("m325tracebw", c(as.list(nx), verbose = TRUE))
#'
#' # Distinct options for opinion aggregation lead to distinct traced
#' # temperature and pressure:
#' output <- list(
#'   mean   = do.call("m325tracebw",
#'                    c(as.list(nx), verbose = FALSE, opinion = "mean")),
#'   median = do.call("m325tracebw",
#'                    c(as.list(nx), verbose = FALSE, opinion = "median"))
#' )
#'
#' stopifnot(
#'   round(
#'     subset(
#'       output$mean,
#'       node == 13 & aggregation == "median",
#'       c("temperature", "pressure", "consumption")
#'     ) - subset(
#'       output$median,
#'       node == 13 & aggregation == "median",
#'       c("temperature", "pressure", "consumption")
#'     ),
#'     5
#'     # difference between aggregation options
#'   ) == c(dt = 0.03732, dp = 0.00139, dg = 0)
#' )
#' @export
m325tracebw <- function(sender = 6, acceptor = 7,
                        temperature = 70.0, pressure = pipenostics::mpa_kgf(6),
                        consumption = 20, d = 100, len = 72.446, year = 1986,
                        insulation = 0, laying = "tunnel", beta = FALSE,
                        exp5k = TRUE, roughness = 1e-3, inlet = .5, outlet = 1.0,
                        method = "romeo", opinion = "median", verbose = TRUE,
                        csv = FALSE, file = "m325tracebw.csv"
                       ){

  # Trace thermal-hydraulic regime  ----
  .func_name <- "m325tracebw"

  # Validate function input ----
  checkmate::assert_true(all(!is.na(acceptor)))
  acceptor <- as.character(acceptor)
  checkmate::assert_true(!any(duplicated(acceptor)))  # only single income edge!
  n <- length(acceptor)
  sender <- as.character(sender)
  checkmate::assert_character(sender, any.missing = FALSE, len = n)
  checkmate::assert_double(
    temperature,
    lower = 0, upper = 350, finite = TRUE, any.missing = TRUE, len = n
  )
  checkmate::assert_double(
    pressure,
    lower = 8.4e-2, upper = 100, finite = TRUE, any.missing = TRUE,
    len = n
  )
  checkmate::assert_double(
    consumption,
    lower = 1e-3, upper = 1e5, finite = TRUE, any.missing = TRUE,
    len = n
  )
  norms <- pipenostics::m325nhldata  # use brief name
  checkmate::assert_double(
    d,
    lower = min(norms$diameter), upper = max(norms$diameter), finite = TRUE,
    any.missing = FALSE, len = n
  )
  checkmate::assert_double(
    len,
    lower = 0, finite = TRUE, any.missing = FALSE, len = n
  )
  checkmate::assert_integerish(
    year,
    lower = 1900L, upper = max(norms$epoch), any.missing = FALSE, len = n
  )
  checkmate::assert_subset(insulation, choices = unique(norms$insulation))
  checkmate::assert_subset(
    laying,
    choices = unique(norms$laying), empty.ok = FALSE
  )
  rm(norms)  # no need in any norms further
  checkmate::assert_logical(beta, any.missing = FALSE, len = n)
  checkmate::assert_logical(exp5k, any.missing = FALSE, len = n)
  checkmate::assert_double(
    roughness,
    lower = 0, upper = .2, any.missing = FALSE, len = n
  )
  checkmate::assert_double(
    outlet,
    lower = 0, finite = TRUE, any.missing = FALSE, len = n
  )
  checkmate::assert_double(
    inlet,
    lower = 0, finite = TRUE, any.missing = FALSE, len = n
  )
  checkmate::assert_choice(method, c("romeo", "vatankhan", "buzelli"))
  checkmate::assert_flag(verbose)
  checkmate::assert_choice(opinion, c("median", "mean"))
  checkmate::assert_flag(csv)
  if (csv) {
    checkmate::assert_character(
      basename(file), pattern = "^[[:alnum:]_\\.\\-]+$",
      any.missing = FALSE, len = 1
    )  # check for validness of file name!
    checkmate::assert_path_for_output(file)
  }
  # Validate method aspects ----
  is_terminal_node <- !(acceptor %in% sender)
  checkmate::assert_double(temperature[is_terminal_node], any.missing = FALSE, min.len = 1)
  checkmate::assert_double(pressure[is_terminal_node], any.missing = FALSE, min.len = 1)
  checkmate::assert_double(consumption[is_terminal_node], any.missing = FALSE, min.len = 1)

  # Conf: list of aggregation functions ----
  agg_func <- list(
    span = function(x) max(x) - min(x), median = stats::median, mean = mean
  )

  # Start backward tracing ----
  time_stamp_posixct <- Sys.time()
  node_state <- with(list(), {
    v <- table(c(sender, acceptor)) - 1L
    v[!(names(v) %in% acceptor)] <- -1L
    # Conventional values:
    #   <0 - already processed,
    #    0 - should be processed,
    #   >0 - not yet processed, number of not processed successors
    v
  })
  if (verbose)
    cat(
      sprintf(
        "\n%s %s | start backward tracing; segments %i;",
        time_stamp_posixct, .func_name, n
      )
    )
  rm(n)

  job_num <- 0L
  # Set up job log columns ----
  job_log <- data.frame(
    node = acceptor, trace = "sensor", backward = TRUE,
    aggregation = "identity", temperature, pressure, consumption, job = job_num
  )[is_terminal_node,]
  rm(is_terminal_node)

  if (csv)
    cat(paste(colnames(job_log), collapse = ","), "\n", file = file)

  # Start up job while-cycle ----
  while (any(node_state == 0L)) {
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | start job; job %i;",
          time_stamp_posixct, .func_name, job_num
        )
      )

    is_node_ready <- node_state == 0L
    ready_nodes <- names(is_node_ready)[is_node_ready]  # acceptor names
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | now process; %i node(s); [%s]",
          time_stamp_posixct, .func_name, length(ready_nodes),
          paste(ready_nodes, collapse = ",")
        )
      )

    # Make up job aggregations ----
    agg_log <- lapply(
      structure(names(agg_func), names = names(agg_func)),
      function(func_name, log_df) {
        trace <- tapply(log_df$trace, log_df$node, "paste", collapse = "|")
        data.frame(
             node = names(trace),
             trace = trace,
             backward = TRUE,
             aggregation = func_name,
             temperature = tapply(log_df$temperature, log_df$node, agg_func[[func_name]]),
             pressure = tapply(log_df$pressure, log_df$node, agg_func[[func_name]]),
             consumption = tapply(log_df$consumption, log_df$node, sum),
             job = job_num
        )
      }  ,
      log_df = job_log[job_log$node %in% ready_nodes &
                       job_log$aggregation == "identity",]

    )

    job_log <- rbind(job_log, do.call("rbind", agg_log))

    # Dump log to file ----
    if (csv)
      utils::write.table(
        job_log[job_log$job == job_num,],
        file = file, append = TRUE, quote = FALSE, sep = ",",
        col.names = FALSE, row.names = FALSE
      )

    # Calculate ThHy-regime ----
    is_processed_pipe <- which(acceptor %in% ready_nodes)
    regime <- job_log[job_log$job == job_num & job_log$aggregation == opinion,]
    checkmate::assert_true(
      all(acceptor[is_processed_pipe] %in% regime[["node"]]) &&
      all(regime[["node"]] %in% acceptor[is_processed_pipe])
    )
    regime_index <- match(acceptor[is_processed_pipe], regime[["node"]])

    # * Temperature drop ----
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | trace temperature;;", time_stamp_posixct, .func_name
        )
      )

    this_sender_temperature <-
      regime[["temperature"]][regime_index] + pipenostics::m325dropt(
        regime[["temperature"]][regime_index],
        regime[["pressure"]][regime_index],
        regime[["consumption"]][regime_index],
        d[is_processed_pipe], len[is_processed_pipe],
        year[is_processed_pipe], insulation[is_processed_pipe],
        laying[is_processed_pipe], beta[is_processed_pipe],
        exp5k[is_processed_pipe]
      )

    # * Pressure drop ----
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | trace pressure;;", time_stamp_posixct, .func_name
        )
      )
    this_sender_pressure <-
      regime[["pressure"]][regime_index] + pipenostics::dropp(
        regime[["temperature"]][regime_index],
        regime[["pressure"]][regime_index],
        regime[["consumption"]][regime_index],
        d[is_processed_pipe]*1e-3,
        len[is_processed_pipe],
        roughness[is_processed_pipe], inlet[is_processed_pipe],
        outlet[is_processed_pipe], method
      )

    # * Consumption loss ----
    this_sender_consumption <- regime[["consumption"]][regime_index] + 0

    rm(regime_index, regime)

    # Log ThHy-regime ----
    job_log <- rbind(
      job_log,
      data.frame(
        node = sender[is_processed_pipe],
        trace = acceptor[is_processed_pipe],
        backward = TRUE,
        aggregation = "identity",
        temperature = this_sender_temperature,
        pressure = this_sender_pressure,
        consumption = this_sender_consumption,
        # All above data is for the next job:
        job = job_num + 1L
      )
    )
    rm(
      this_sender_temperature, this_sender_pressure, this_sender_consumption,
      is_processed_pipe
    )

    # Recalculate node state ----
    node_state[is_node_ready] <- node_state[is_node_ready] - 1L
    n <- table(sender[acceptor %in% ready_nodes])
    node_state[names(n)] <- node_state[names(n)] - n
    rm(ready_nodes, n)
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | finish job; job %i; processed node(s) %i",
          time_stamp_posixct, .func_name, job_num, sum(is_node_ready)
        )
      )
    job_num <- job_num + 1L
  }  # end while
  rm(is_node_ready)

  # Log last cycle data ----
  if (csv)
    utils::write.table(
      job_log[job_log$job == job_num & job_log$node %in% acceptor,],
      file = file, append = TRUE, quote = FALSE, sep = ",",
      col.names = FALSE, row.names = FALSE
    )

  if (verbose)
    cat(
      sprintf(
        "\n%s %s | finish backward tracing;;\n",
        time_stamp_posixct, .func_name
      )
    )
  # Finish backward tracing ----
  job_log[job_log$job != job_num, ]
}
