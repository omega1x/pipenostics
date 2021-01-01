#' @title
#'  Minenergo-325. Trace forwards thermal-hydraulic regime for district heating
#'  network
#'
#' @family Regime tracing
#'
#' @description
#'  Trace values of thermal-hydraulic regime (temperature, pressure,
#'  consumption) in bunched pipeline along the flow direction using norms of
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
#'  Tracing starts from sensor-equipped root node and goes forward, i.e along
#'  the flow direction.
#'
#'  In the forward tracing algorithm they assume the flow of heat carrier is
#'  distributed proportionally to the cross-sectional area of the outgoing
#'  pipeline. Actually, a lot of reasons may cause significant deviations from
#'  this assumption. As a result, the sequence of paired backward/forward
#'  tracing may be divergent for regime parameters.
#'
#'  Though some input arguments are natively vectorized their individual values
#'  all relate to common part of district heating network, i.e. associated with
#'  common object. It is due to isomorphism between vector representation and
#'  directed graph of this network. For more details of isomorphic topology
#'  description see \code{\link{m325testbench}}.
#'
#'  They are welcome to couple the algorithm with functionality of data.table.
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
#'    (water) sensor-measured on the root node, [\emph{°C}].
#'    Use \code{NA_float_}s for nodes without temperature sensor.
#'    Type: \code{[double]}.
#'
#' @param pressure
#'    \emph{snapshot of thermal-hydraulic regime state}: sensor-measured
#'    \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'    of heat carrier (water) inside the pipe (i.e. acceptor's incoming edge),
#'    [\emph{MPa}]. Type: \code{[double]}. Use \code{NA_float_}s for nodes
#'    without pressure sensor.
#'
#' @param consumption
#'    \emph{snapshot of thermal-hydraulic regime state}:
#'    sensor-measured amount of heat carrier (water) on root node that is
#'    transferred by pipe (i.e. acceptor's incoming edge) during a period,
#'    [\emph{ton/hour}]. Type: \code{[double]}. Use \code{NA_float_}s for nodes
#'    without consumption sensor.
#'
#' @param d
#'    internal diameter of pipe (i.e.diameter of acceptor's incoming edge),
#'    [\emph{mm}].
#'    Type: \code{[double]}.
#'
#' @param len
#'
#' @param year
#' @param insulation
#' @param laying
#' @param beta
#' @param exp5k
#' @param roughness
#' @param inlet
#' @param outlet
#' @param elev_tol
#' @param method
#' @param verbose
#' @param csv
#' @param file
#'
#' @return
#' @export
#'
#' @examples
m325tracefw <- function(sender = c(0, 1), acceptor = c(1, 2),
                        temperature = c(70.0, NA_real_),
                        pressure = c(pipenostics::mpa_kgf(6), NA_real_),
                        consumption = c(20, NA_real_),
                        d = rep_len(100, 2), len = rep_len(72.446, 2),
                        year = rep_len(1986, 2), insulation = rep_len(0, 2),
                        laying = rep_len("tunnel", 2), beta = rep_len(FALSE, 2),
                        exp5k = rep_len(TRUE, 2), roughness = rep_len(1e-3, 2),
                        inlet = c(.5, 1), outlet = c(1.0, 1),
                        elev_tol = 0.1,
                        method = "romeo", verbose = TRUE,
                        csv = FALSE, file = "m325tracefw.csv") {

  # Perform forward tracing ----
  .func_name <- "m325tracefw"

  # Assertions ----
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
  checkmate::assert_flag(csv)
  if (csv) {
    checkmate::assert_character(
      basename(file), pattern = "^[[:alnum:]_\\.\\-]+$",
      any.missing = FALSE, len = 1
    )  # check for validness of file name!
    checkmate::assert_path_for_output(file)
  }

  # Configuration ----
  time_stamp_posixct <- Sys.time()

  if (verbose)
    cat(
      sprintf(
        "\n%s %s | start forward tracing; segments %i;",
        time_stamp_posixct, .func_name, n
      )
    )
  rm(n)

  # Compute discharges ----
  discharge <-
    setNames(1 - d ^ 2 / tapply(d ^ 2, sender, sum)[sender], acceptor)

  # List search paths ----
  tracing_path <- flowls(sender, acceptor)
  checkmate::assert_list(
    tracing_path,
    types = "integerish", any.missing = FALSE,
    min.len = 1, max.len = length(acceptor)
  )

  root_node <- tracing_path[[1]][[1]]
  checkmate::assert_count(root_node, positive = TRUE)

  # Validate initial data ----
  checkmate::assert_double(temperature[[root_node]], any.missing = FALSE, len = 1)
  checkmate::assert_double(pressure[[root_node]], any.missing = FALSE, len = 1)
  checkmate::assert_double(consumption[[root_node]], any.missing = FALSE, len = 1)


  job_log <- data.frame(
    node = acceptor[root_node],
    trace = "sensor",
    backward = FALSE,
    aggregation = "identity",
    temperature = temperature[root_node],
    pressure = pressure[root_node],
    consumption = consumption[root_node],
    job = 0L
  )

  if (csv)
    utils::write.table(
      job_log,
      file = file, append = FALSE, quote = FALSE, sep = ",",
      col.names = TRUE, row.names = FALSE
    )

  # Trace searched paths ----
  for (job_num in seq_along(tracing_path)) {
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | now process; %i node(s); [%s]",
          time_stamp_posixct, .func_name, length(tracing_path[[job_num]]),
          paste(acceptor[tracing_path[[job_num]]], collapse = ",")
        )
      )

    current_path <- tracing_path[[job_num]][-1]
    checkmate::assert_integer(
      current_path,
      lower = 1L, upper = length(acceptor), any.missing = FALSE,
      min.len = 1, max.len = length(acceptor), unique = TRUE
    )

    regime <- m325traceline(
      temperature[root_node], pressure[root_node], consumption[root_node],
      discharge[current_path], d[current_path], len[current_path],
      year[current_path], insulation[current_path], laying[current_path],
      beta[current_path], exp5k[current_path], roughness[current_path],
      inlet[current_path], outlet[current_path],
      elev_tol = elev_tol,
      method = method,
      forward = TRUE,
      absg = FALSE
    )
    regime <- as.data.frame(regime)
    regime$node <- acceptor[current_path]
    regime$trace <- sender[current_path]
    regime$backward <- FALSE
    regime$aggregation <- "identity"
    regime$job <- job_num
    job_log <- rbind(job_log, regime)

    if (csv)
      utils::write.table(
        job_log[job_log$job == job_num,],
        file = file, append = TRUE, quote = FALSE, sep = ",",
        col.names = FALSE, row.names = FALSE
      )
  }

  if (verbose)
    cat(
      sprintf(
        "\n%s %s | finish forward tracing;;\n",
        time_stamp_posixct, .func_name
      )
    )
  job_log
}

# Minimum two nodes should be in district heating network graph:
m325tracefw(verbose = FALSE)

# node  trace backward aggregation temperature  pressure consumption job
# 1    1 sensor    FALSE    identity    70.00000 0.5883990          20   0
# 2    2      1    FALSE    identity    69.71603 0.5813153          20   1

# Example with the test bench:
nx <- pipenostics::m325testbench

# avoid using numeric identifiers for nodes:
nx$sender <- paste0("N", nx$sender)
nx$acceptor <- paste0("N", nx$acceptor)

# Alter units:
nx$d <- 1e3 * nx$d  # convert [m] to [mm]

# Perform backward tracing to get regime on root node:
bw_report <- do.call("m325tracebw", c(as.list(nx), verbose = FALSE))

# Put the traced values to the root node of the test bench:
root_node_idx <- 12
root_node <- paste0("N", root_node_idx)
regime_param  <- c("temperature", "pressure", "consumption")
nx[root_node_idx, regime_param] <-
  subset(bw_report,
         node == root_node & aggregation == "median",
         regime_param)
rm(root_node, root_node_idx)

# Trace the test bench forward for the first time:
fw_report <- do.call("m325tracefw",
                     c(as.list(nx), verbose = FALSE, elev_tol = .5))

# Let's compare traced regime at terminal nodes back to test bench:
report <- subset(
    rbind(bw_report, fw_report),
    node %in% subset(nx, !(acceptor %in% sender))$acceptor &
      aggregation == "identity"
)

regime_delta <- colMeans(
  subset(report, backward, regime_param) -
  subset(report, !backward, regime_param)
)
print(regime_delta)
# temperature      pressure   consumption
# -4.640201e-01 -5.208802e-03 -5.465713e-16

stopifnot(sqrt(regime_delta %*% regime_delta) < 0.5)

