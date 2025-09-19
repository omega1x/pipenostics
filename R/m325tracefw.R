#' @title
#'  Minenergo-325. Massively trace forwards thermal-hydraulic regime for
#'  district heating network
#'
#' @family Regime tracing
#'
#' @description
#'  Trace values of thermal-hydraulic regime (temperature, pressure,
#'  flow rate, and other) in the bunched pipeline along the flow direction using
#'  norms of heat loss values prescribed by
#'  \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' @details
#'  The calculated (values of) regime may be considered as representation of
#'  district heating process in conditions of hypothetically perfect
#'  technical state of pipe walls and insulation.
#'
#'  They consider the topology of district heating network represented by
#'  \code{\link{m325nxdata}}:
#'
#'  \figure{m325tracefw.png}
#'
#'  Tracing starts from sensor-equipped root node and goes forward, i.e along
#'  the flow direction. Function \code{\link{m325traceline}} serves under the
#'  hood for tracing identified linear segments from root node to every
#'  terminal node. Hence they only need root node to be equipped with sensors.
#'  Sensors at other nodes are redundant in forward tracing, since the tracing
#'  algorithm by no means consider them for tracing.
#'
#'  Moreover in the forward tracing algorithm they assume the flow of heat
#'  carrier is distributed proportionally to the cross-sectional area of the
#'  outgoing pipeline. Actually, a lot of reasons may cause significant
#'  deviations from this assumption. As a result, the sequence of paired
#'  backward/forward tracing may be divergent for regime parameters.
#'
#'  Though some input arguments are natively vectorized their individual values
#'  all relate to common part of district heating network, i.e. associated with
#'  common object. It is due to isomorphism between vector representation and
#'  directed graph of this network. For more details of isomorphic topology
#'  description see \code{\link{m325nxdata}}.
#'
#'  They are welcome to couple the algorithm with functionality of
#'  \href{https://CRAN.R-project.org/package=data.table}{data.table}.
#'
#'  Optional verification of pipe diameters and wall thicknesses is performed
#'  against \code{\link{b36pipedata}} data.
#'
#' @param sender
#'   identifier of the node which heat carrier flows out.
#'   Type: any type that can be painlessly coerced to character by
#'   \code{\link{as.character}}.
#'
#' @param acceptor
#'   identifier of the node which heat carrier flows in. According to topology
#'   of test bench considered this identifier should be unique for every row.
#'   Type: any type that can be painlessly coerced to character by
#'   \code{\link{as.character}}.
#'
#' @param temperature
#'   sensor-measured temperature of heat carrier (water) sensor-measured on
#'   the root node, [\emph{°C}].
#'   Use \code{NA_float_}s for nodes without temperature sensor.
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param pressure
#'   sensor-measured
#'   \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{
#'    absolute pressure} of heat carrier (water) inside the pipe on the root
#'   node, [\emph{MPa}]. Use \code{NA_float_}s for nodes without pressure
#'   sensor. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param flow_rate
#'   sensor-measured amount of heat carrier (water) on root node that is
#'   transferred by pipe during a period, [\emph{ton/h}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'   Use \code{NA_float_}s for nodes without flow rate sensor.
#'
#' @param a
#'   heat carrier (water) volume loss factor of cylindrical pipe,
#'   [\emph{h⁻¹}]. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param d
#'   nominal (outside) diameter of pipe (i.e.diameter of acceptor's incoming
#'   edge), [\emph{mm}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'   nominal wall thickness of pipe, [\emph{mm}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param len
#'   pipe length (i.e. length of acceptor's incoming edge), [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param year
#'   year when the pipe (i.e. acceptor's incoming edge) is put in operation
#'   after laying or total overhaul.
#'   Type: \code{\link[checkmate]{assert_integerish}}.
#'
#' @param insulation
#'   identifier of insulation that covers the exterior of pipe (i.e. acceptor's
#'   incoming edge):
#'    \describe{
#'      \item{\code{0}}{no insulation}
#'      \item{\code{1}}{foamed polyurethane or analogue}
#'      \item{\code{2}}{polymer concrete}
#'    }
#'   Type: \code{\link[checkmate]{assert_subset}}.
#'
#' @param laying
#'   type of pipe laying depicting the position of pipe in space. Only five
#'   types of pipe laying are considered:
#'   \itemize{
#'     \item \code{air},
#'     \item \code{channel},
#'     \item \code{room},
#'     \item \code{tunnel},
#'     \item \code{underground}.
#'   }
#'   Type: \code{\link[checkmate]{assert_subset}}.
#'
#' @param beta
#'   logical indicator: should they consider additional heat loss of fittings
#'   located on this pipe (i.e. acceptor's incoming edge)?
#'   Type: \code{\link[checkmate]{assert_logical}}.
#'
#' @param exp5k
#'   logical indicator for regime of pipe (i.e. acceptor's incoming edge): if
#'   \code{TRUE} pipe is operated more that \code{5000} hours per year.
#'   Type: \code{\link[checkmate]{assert_logical}}.
#'
#' @param roughness
#'   roughness of internal wall of pipe (i.e. acceptor's incoming edge),
#'   [\emph{m}]. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param inlet
#'   elevation of pipe inlet, [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param outlet
#'   elevation of pipe outlet, [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param elev_tol
#'   maximum allowed discrepancy between adjacent outlet and inlet elevations
#'   of two subsequent pipes in the traced path, [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param method
#'   method of determining \emph{Darcy friction factor}:
#'   \itemize{
#'     \item \code{romeo}
#'     \item \code{vatankhah}
#'     \item \code{buzzelli}
#'   }
#'   Type: \code{\link[checkmate]{assert_choice}}.
#'   For more details see \code{\link{dropp}}.
#'
#' @param strict_sizes
#'   verify diameter and wall thickness with the actual pipe specifications
#'   produced. Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @param verbose
#'   logical indicator: should they watch tracing process on console?
#'   Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @param csv
#'   logical indicator: should they incrementally dump results to \emph{csv}-
#'   file while tracing?
#'   Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @param file
#'   name of \emph{csv}-file which they dump results to.
#'   Type: \code{\link[checkmate]{assert_character}} of length 1 that can be
#'   used safely to create a file and write to it.
#'
#' @param use_cluster
#'   utilize functionality of parallel processing on multi-core CPU.
#'   Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @return
#'   A \code{\link{data.frame}} containing results (detailed log) of tracing in
#'   \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{narrow format}:
#'   \describe{
#'      \item{\code{node}}{
#'        \emph{Tracing job}. Identifier of the node which regime parameters is
#'        calculated for. Values in this vector are identical to those in
#'        argument \code{acceptor}.
#'        Type: \code{\link[checkmate]{assert_character}}.
#'      }
#'
#'      \item{\code{tracing}}{
#'        \emph{Tracing job}. Identifiers of nodes from which regime parameters
#'        are traced for the given node. Identifier \code{sensor} is used when
#'        values of regime parameters for the node are sensor readings.
#'        Type: \code{\link[checkmate]{assert_character}}.
#'      }
#'
#'      \item{\code{backward}}{
#'        \emph{Tracing job}. Identifier of tracing direction. It constantly
#'        equals to \code{FALSE}.
#'        Type: \code{\link[checkmate]{assert_logical}}.
#'      }
#'
#'      \item{\code{aggregation}}{
#'        \emph{Tracing job}. Identifier of the aggregation method associated
#'        with traced values. For forward tracing the only option is
#'        \code{identity}.
#'        Type: \code{\link[checkmate]{assert_character}}.
#'      }
#'      \item{\code{loss}}{
#'         \emph{Traced thermal hydraulic regime}. Normative specific heat loss
#'         power of adjacent pipe, [\emph{kcal/m/h}].
#'         Type: \code{\link[checkmate]{assert_double}}.
#'      }
#'      \item{\code{flux}}{
#'         \emph{Traced thermal hydraulic regime}. Normative heat flux of
#'         adjacent pipe, [\emph{W/m²}].
#'         Type: \code{\link[checkmate]{assert_double}}.
#'      }
#'      \item{\code{Q}}{
#'        \emph{Traced thermal hydraulic regime}. Normative heat loss of
#'        adjacent pipe per day, [\emph{kcal}].
#'        Type: \code{\link[checkmate]{assert_character}}.
#'      }
#'      \item{\code{temperature}}{
#'        \emph{Traced thermal hydraulic regime}. Traced temperature of heat
#'        carrier (water) that is associated with the node, [\emph{°C}].
#'        Type: \code{\link[checkmate]{assert_double}}.
#'      }
#'
#'      \item{\code{pressure}}{
#'        \emph{Traced thermal hydraulic regime}. Traced pressure of heat
#'        carrier (water) that is associated with the node, [\emph{MPa}].
#'        Type: \code{\link[checkmate]{assert_double}}.
#'      }
#'
#'      \item{\code{flow_rate}}{
#'         \emph{Traced thermal hydraulic regime}. Traced flow rate of heat
#'         carrier (water) that is associated with the node, [\emph{ton/h}].
#'         Type: \code{\link[checkmate]{assert_double}}.
#'      }
#'
#'      \item{\code{job}}{
#'         \emph{Tracing job}. Value of tracing job counter.
#'         For forward tracing value of \code{job} counts the number of traced
#'         paths from root node. Type: \code{\link[checkmate]{assert_count}}.
#'      }
#'   }
#'   Type: \code{\link[checkmate]{assert_data_frame}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # Minimum two nodes should be in district heating network graph:
#' m325tracefw(verbose = FALSE)
#'
#' # Consider isomorphic representation of District Heating Network graph:
#' DHN <- m325nxdata
#'
#' # * avoid using numeric identifiers for nodes:
#' DHN$sender   <- sprintf("N%02i", DHN$sender)
#' DHN$acceptor <- sprintf("N%02i", DHN$acceptor)
#'
#' # Perform backward tracing to get regime on root node:
#' bw_report <- do.call("m325tracebw", c(as.list(DHN), verbose = FALSE))
#'
#' # Put the traced values to the root node of test bench:
#' root_node_idx <- 12
#' root_node <- sprintf("N%02i", root_node_idx)
#' regime_param  <- c("temperature", "pressure", "flow_rate")
#' DHN[root_node_idx, regime_param] <-
#'   subset(bw_report,
#'          node == root_node & aggregation == "median",
#'          regime_param)
#' rm(root_node, root_node_idx)
#'
#' # Trace the test bench forward for the first time:
#' fw_report <- do.call(
#'   "m325tracefw", c(as.list(DHN), verbose = FALSE, elev_tol = .5)
#' )
#'
#' # Let's compare traced regime at terminal nodes back to test bench:
#' report <- subset(
#'   rbind(bw_report, fw_report),
#'   node %in% subset(DHN, !(acceptor %in% sender))$acceptor &
#'     aggregation == "identity"
#' )
#'
#' regime_delta <- colMeans(
#'   subset(report, backward, regime_param) -
#'     subset(report, !backward, regime_param)
#' )
#' print(regime_delta)
#'
#' stopifnot(sqrt(regime_delta %*% regime_delta) < 0.5)
#'
#' # To address the problem of possible norm losses of the heat carrier,
#' # they could roughly define the leaks as follows:
#' DHN[, "a"] <- 0.0025 * (365 - 90) / 365
#'
#' # so that forward tracing
#' fw_report_loss <- do.call(
#'   "m325tracefw", c(as.list(DHN), verbose = FALSE, elev_tol = .5)
#' )
#'
#' # produces slightly different results [kcal/m/h]:
#' max(abs(fw_report[, "loss"] - fw_report_loss[, "loss"]), na.rm = TRUE)
#'
#' @export
m325tracefw <- function(
  sender = c(0, 1),  acceptor = c(1, 2),

  temperature = c(70.0, NA_real_),
  pressure = c(pipenostics::mpa_kgf(6), NA_real_),
  flow_rate = c(20, NA_real_), a = c(0, 0),

  d = c(100, 100), wth = c(12, 12), len = c(72.446, 72.446),
  year = c(1986, 1986), insulation = c(0, 0),
  laying = c("tunnel", "tunnel"),
  beta = c(FALSE, FALSE), exp5k = c(TRUE, TRUE),
  roughness = c(1e-3, 1e-3),
  inlet = c(.5, 1), outlet = c(1.0, 1), elev_tol = 0.1,
  method = "romeo", strict_sizes = FALSE,
  verbose = TRUE, csv = FALSE, file = "m325tracefw.csv", use_cluster = FALSE
) {
  # Perform forward tracing ----
  .func_name <- "m325tracefw"

  # Assertions ----
  checkmate::assert_vector(
    acceptor, any.missing = FALSE, min.len = 1, unique = TRUE
  )
  n <- length(acceptor)
  checkmate::assert_vector(sender, any.missing = FALSE, len = n)
  checkmate::assert_double(
    temperature,
    lower = 0, upper = 350, finite = TRUE, any.missing = TRUE, len = n
  )
  checkmate::assert_double(
    pressure,
    lower = 8.4e-2, upper = 100, finite = TRUE, any.missing = TRUE, len = n
  )
  checkmate::assert_double(
    flow_rate,
    lower = 1e-3, upper = 1e5, finite = TRUE, any.missing = TRUE, len = n
  )
  checkmate::assert_double(
    a,
    lower = 0, upper = 25e-4, finite = TRUE, any.missing = FALSE, len = n
  )
  checkmate::assert_double(
    d,
    lower = min(pipenostics::m325nhldata[["d"]]),
    upper = max(pipenostics::m325nhldata[["d"]]),
    finite = TRUE, any.missing = FALSE, len = n
  )
  checkmate::assert_double(
    wth,
    lower = min(pipenostics::b36pipedata[["wth"]]),
    upper = max(pipenostics::b36pipedata[["wth"]]),
    finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    len,
    lower = 0, finite = TRUE, any.missing = FALSE, len = n
  )
  checkmate::assert_integerish(
    year,
    lower = 1900L, upper = max(pipenostics::m325nhldata[["epoch"]]),
    any.missing = FALSE, len = n
  )
  checkmate::assert_subset(
    insulation,
    choices = unique(pipenostics::m325nhldata[["insulation"]])
  )
  checkmate::assert_subset(
    laying,
    choices = unique(pipenostics::m325nhldata[["laying"]]), empty.ok = FALSE
  )
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
  checkmate::assert_number(elev_tol, lower = 0, upper = 10, finite = TRUE)
  checkmate::assert_choice(method, c("romeo", "vatankhah", "buzzelli"))
  checkmate::assert_flag(strict_sizes)
  checkmate::assert_flag(verbose)
  checkmate::assert_flag(csv)
  if (csv) {
    checkmate::assert_character(
      basename(file),
      pattern = "^[[:alnum:]_\\.\\-]+$",  any.missing = FALSE, len = 1
    )  # check for validness of file name!
    checkmate::assert_path_for_output(file)
  }
  checkmate::assert_true(commensurable(c(
    length(sender), length(acceptor), length(temperature), length(pressure),
    length(flow_rate), length(d), length(wth), length(len),
    length(year), length(insulation), length(laying), length(beta),
    length(exp5k), length(roughness), length(inlet), length(outlet)
  )))
  checkmate::assert_true(all(d - 2 * wth > 0.5))  # in mm
  if (strict_sizes) checkmate::assert_true(all(pipenostics::b36dwthv(d, wth)))

  acceptor <- as.character(acceptor)
  sender <- as.character(sender)

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
  discharge <- structure(
    1 - d^2 / tapply(d^2, sender, sum)[sender], names = acceptor
  )

  # List search paths ----
  tracing_path <- pipenostics::flowls(sender, acceptor, use_cluster)
  checkmate::assert_list(
    tracing_path,
    types = "integerish", any.missing = FALSE, min.len = 1,
    max.len = length(acceptor)
  )

  root_node <- tracing_path[[1]][[1]]
  checkmate::assert_count(root_node, positive = TRUE)

  # Validate initial data ----
  checkmate::assert_double(
    temperature[[root_node]],
    any.missing = FALSE, len = 1L
  )
  checkmate::assert_double(
    pressure[[root_node]],
    any.missing = FALSE, len = 1L
  )
  checkmate::assert_double(
    flow_rate[[root_node]],
    any.missing = FALSE, len = 1L
  )

  job_log <- data.frame(
    node        = acceptor[root_node],
    tracing     = "sensor",
    backward    = FALSE,
    aggregation = "identity",
    loss        = NA_real_,
    flux        = NA_real_,
    Q           = NA_real_,
    temperature = temperature[root_node],
    pressure    = pressure[root_node],
    flow_rate   = flow_rate[root_node],
    job         = 0L
  )

  if (csv)
    utils::write.table(
      job_log,
      file = file,  append = FALSE, quote   = FALSE, sep       = ",",
      col.names = TRUE, row.names = FALSE
    )

  # Trace searched paths ----
  for (job_num in seq_along(tracing_path)) {
    if (verbose)
      cat(
        sprintf(
          "\n%s %s | now process; %i node(s); [%s]",
          time_stamp_posixct,
          .func_name,
          length(tracing_path[[job_num]]),
          paste(acceptor[tracing_path[[job_num]]], collapse = ",")
        )
      )

    current_path <- tracing_path[[job_num]][-1]
    checkmate::assert_integer(
      current_path,
      lower   = 1L, upper   = length(acceptor), any.missing = FALSE,
      min.len = 1,  max.len = length(acceptor), unique      = TRUE
    )
    regime <- m325traceline(
      temperature = temperature[root_node],
      pressure    = pressure[root_node],
      flow_rate   = flow_rate[root_node],
      g           = discharge[current_path],
      a           = a[current_path],
      d           = d[current_path],
      wth         = wth[current_path],
      len         = len[current_path],
      year        = year[current_path],
      insulation  = insulation[current_path],
      laying      = laying[current_path],
      beta        = beta[current_path],
      exp5k       = exp5k[current_path],
      roughness   = roughness[current_path],
      inlet       = inlet[current_path],
      outlet      = outlet[current_path],
      elev_tol    = elev_tol[[1]],
      method      = method,
      forward     = TRUE,
      absg        = FALSE
    )
    regime <- as.data.frame(regime)

    regime[["node"]]        <- acceptor[current_path]
    regime[["tracing"]]     <- sender[current_path]
    regime[["backward"]]    <- FALSE
    regime[["aggregation"]] <- "identity"
    regime[["job"]]         <- job_num

    job_log <- rbind(job_log, regime)
    job_log <- job_log[
      !duplicated(job_log[, setdiff(colnames(job_log), "job")]),
    ]

    if (csv)
      utils::write.table(
        job_log[job_log[["job"]] == job_num, ],
        file      = file,
        append    = TRUE,
        quote     = FALSE,
        sep       = ",",
        col.names = FALSE,
        row.names = FALSE
      )
  }

  if (verbose)
    cat(sprintf(
      "\n%s %s | finish forward tracing;;\n",
      time_stamp_posixct,
      .func_name
    ))
  job_log
}
