#' @title
#'  Trace thermal-hydraulic regime for linear segment of district
#'  heating network
#'
#' @family Regime tracing
#'
#' @description
#'  Trace values of thermal-hydraulic regime (temperature, pressure,
#'  flow_rate, and other) along the adjacent linear segments of pipeline using
#'  user-provided values of \emph{specific heat loss power}.
#'
#' @details
#'  They consider only simple tracing paths which do not contain rings and any
#'  kind of parallelization. At the same time bidirectional (forward and
#'  backward) tracing is possible in accordance with sensor position. They also
#'  may consider discharges to network at the inlet of each pipeline segment
#'  as an approximation of actual forks of flows. Relevant illustration of
#'  adopted assumptions for 4-segment tracing path is depicted on the next
#'  figure.
#'
#'  \figure{regtrace.png}
#'
#'  They make additional check for consistency of \code{inlet} and \code{outlet}
#'  values for subsequent pipe segments. Discrepancy of appropriate elevations
#'  cannot be more than \code{elev_tol}.
#'
#'  Since inner diameter of the pipe is used as input, the the thickness of the
#'  pipe wall additionally considered in heat flux calculations. Pipe wall
#'  thickness is derived from pipe diameter using
#'  \href{https://docs.cntd.ru/document/1200174717}{GOST 30732} specifications.
#'
#' @param temperature
#'  \emph{Traced thermal hydraulic regime}. Sensor-measured temperature of heat
#'  carrier (water)
#'  inside the pipe sensor-measured at the inlet
#'  (forward tracing) or at the outlet (backward tracing) of path, [\emph{°C}].
#'  Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param pressure
#'  \emph{Traced thermal hydraulic regime}. Sensor-measured
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute
#'  pressure} of heat carrier (water) sensor-measured at the inlet
#'  (forward tracing) or at the outlet (backward tracing) of path, [\emph{MPa}].
#'  Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param flow_rate
#'  \emph{Traced thermal hydraulic regime}. Amount of heat carrier (water)
#'  sensor-measured at the inlet (forward tracing) or at the outlet (backward
#'  tracing) of path, [\emph{ton/h}].
#'  Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param g
#'  amount of heat carrier discharge to network for each pipe segment in the
#'  tracing path enumerated along the direction of flow. If flag \code{absg}
#'  is \code{TRUE} then they treat argument \code{g} as absolute value in
#'  [\emph{ton/h}], otherwise they do as percentage of flow_rate in the
#'  pipe segment.
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param d
#'  outside diameters of subsequent pipes in tracing path that are enumerated
#'  along the direction of flow, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'  wall thickness of pipe, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param len
#'  length of subsequent pipes in tracing path that are enumerated
#'  along the direction of flow, [\emph{m}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param loss
#'  user-provided value of \emph{specific heat loss} power for each pipe in
#'  tracing path enumerated along the direction of flow, [\emph{kcal/m/h}].
#'  Values of the argument can be obtained experimentally, or taken from
#'  regulatory documents. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param roughness
#'  roughness of internal wall for each pipe in tracing path enumerated along
#'  the direction of flow, [\emph{m}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param inlet
#'  elevation of pipe inlet for each pipe in tracing path enumerated along
#'  the direction of flow, [\emph{m}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param outlet
#'  elevation of pipe outlet for each pipe in tracing path enumerated along
#'  the direction of flow, [\emph{m}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param method
#'  method of determining \emph{Darcy friction factor}
#'  \itemize{
#'    \item \code{romeo}
#'    \item \code{vatankhah}
#'    \item \code{buzzelli}
#'  }
#'  Type: \code{\link[checkmate]{assert_choice}}.
#'  For more details see \code{\link{dropp}}.
#'
#' @param elev_tol
#'  maximum allowed discrepancy between adjacent outlet and inlet elevations of
#'  two subsequent pipes in the traced path, [\emph{m}].
#'  Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param forward
#'  tracing direction flag: is it a forward direction of tracing?
#'  If \code{FALSE} the backward tracing is performed.
#'  Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @param absg
#'  Whether argument \code{g} (amount of heat carrier discharge to network) is
#'  an absolute value in [\emph{ton/h}] (\code{TRUE}) or is it a percentage of
#'  flow rate in the pipe segment (\code{FALSE})?
#'  Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @return
#'  \code{\link{list}} containing results (detailed log) of tracing for each
#'  pipe in tracing path enumerated along the direction of flow:
#'  \describe{
#'    \item{\code{temperature}}{
#'      \emph{Traced thermal hydraulic regime}. Traced temperature of heat
#'       carrier (water), [\emph{°C}].
#'       Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{\code{pressure}}{
#'      \emph{Traced thermal hydraulic regime}. Traced pressure of heat
#'       carrier (water) for each pipe in tracing path enumerated along the
#'       direction of flow, [\emph{MPa}].
#'       Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{\code{flow_rate}}{
#'      \emph{Traced thermal hydraulic regime}. Traced flow rate of heat
#'       carrier (water) for each pipe in tracing path enumerated along the
#'       direction of flow, [\emph{ton/h}].
#'       Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'   \item{\code{loss}}{
#'      User-provided specific heat
#'      loss power for each pipe in tracing path enumerated along the direction
#'      of flow, [\emph{kcal/m/h}], - copy of input.
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'   \item{\code{flux}}{
#'      Heat flux for each pipe
#'      in tracing path enumerated along the direction of flow, [\emph{W/m²}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{\code{Q}}{
#'      Heat loss for each
#'      pipe in tracing path enumerated along the direction of flow per day,
#'      [\emph{kcal}]. Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'  }
#'  Type: \code{\link[checkmate]{assert_list}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # Consider 4-segment tracing path.
#' # First, let sensor readings for forward tracing:
#' t_fw <- 130         # [°C]
#' p_fw <- mpa_kgf(6)  # [MPa]
#' g_fw <- 250         # [ton/h]
#'
#' # Let discharges to network for each pipeline segment are somehow determined
#' # as
#' discharges <- seq(0, 30, 10)  # [ton/h]
#'
#' # Experimentally obtained values of specific heat loss power are
#' actual_loss <- c(348.0000, 347.1389, 346.3483, 345.8610)  # [kcal/m/h]
#'
#' # Then the calculated regime (red squares) for forward tracing is
#' traceline(
#'   t_fw, p_fw, g_fw, discharges, loss = actual_loss, forward = TRUE
#' )
#'
#' # Next consider values of traced regime as sensor readings for backward
#' # tracing:
#' t_bw <- 127.3367  # [°C]
#' p_bw <- .5870330  # [MPa]
#' g_bw <- 190       # [ton/h]
#'
#' # Then the calculated regime (red squares) for backward tracing is
#' regime_bw <- traceline(
#'   t_bw, p_bw, g_bw, discharges, loss = actual_loss, forward = FALSE
#' )
#' print(regime_bw)
#'
#' # Let compare sensor readings with backward tracing results:
#' with(regime_bw, {
#'   lambda <- function(val, constraint)
#'     c(val, constraint, constraint - val,
#'       abs(constraint - val) * 100/constraint)
#'   first <- 1
#'   structure(
#'     rbind(
#'       lambda(temperature[first], t_fw),
#'       lambda(pressure[first],    p_fw),
#'       lambda(flow_rate[first], g_fw)
#'     ),
#'     dimnames = list(
#'       c("temperature", "pressure", "flow_rate"),
#'       c("sensor.value", "traced.value", "abs.discr", "rel.discr")
#'     )
#'   )
#' })
#'
#' @export
traceline <- function(
  temperature = 130, pressure = mpa_kgf(6), flow_rate = 250, g = 0,

  d = 720,  wth = 12, len = c(600, 530, 300, 350),
  loss = c(348, 347.1389, 346.3483, 345.861), roughness = 6e-3,

  inlet = 0., outlet = 0., elev_tol = .1,

  method = "romeo", forward = TRUE, absg = TRUE
) {
  pow   <- .Primitive("^")
  DAY   <- 24     # [hours]
  METER <-  1e-3  # [m/mm]

  checkmate::assert_number(
    temperature,
    lower = 0, upper = 350, finite = TRUE
  )
  checkmate::assert_number(
    pressure,
    lower = 8.4e-2, upper = 100, finite = TRUE
  )
  checkmate::assert_number(
    flow_rate,
    lower = 1e-3, upper = 1e5, finite = TRUE
  )
  checkmate::assert_flag(absg)
  checkmate::assert_double(
    g,
    lower = 0, upper = c(1, flow_rate)[[1 + absg]], finite = TRUE,
    any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = 25, upper = 1400, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    wth,
    lower = 0.3, upper = 90, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    len,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    loss,
    lower = 0, upper = 1500, any.missing = FALSE, min.len     = 1L
  )
  checkmate::assert_double(
    roughness,
    lower = 0, upper = .2, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    inlet,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    outlet,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(len), length(loss), length(roughness), length(inlet),
    length(outlet)
  )))
  checkmate::assert_true(all(d - 2 * wth > 0.5))  # in mm
  checkmate::assert_number(
    elev_tol,
    lower = 0, upper = 10, finite = TRUE
  )
  checkmate::assert_choice(method, c("romeo", "vatankhah", "buzzelli"))
  checkmate::assert_flag(forward)

  dh <- outlet - inlet
  checkmate::assert_true(all(abs(dh) < len))  # geometric constraint
  checkmate::assert_true(all(abs(
    utils::tail(inlet, -1) - utils::head(outlet, -1)
  ) < elev_tol))  # elevation consistency of tracing path

  # material balance constraint
  if (forward)
    checkmate::assert_true(flow_rate - sum(g) >= 1e-3)

  with(
    data.frame(
      g, d, wth, len, roughness, inlet, outlet,

      # calculated regime parameters:
      t_regime = NA_real_,  p_regime = NA_real_,  g_regime = NA_real_,
      Q_regime = NA_real_,  loss_regime = NA_real_,  flux_regime = NA_real_
    ),
    {
      # declare temporary scalars for accumulation along the tracing path:
      t_value <- temperature
      p_value <- pressure
      g_value <- flow_rate
      Q_value <- loss_value <- NA_real_

      pipe_enum <- with(list(enum = seq_along(g)), {
        if (forward) enum else rev(enum)
      })
      for (i in pipe_enum) {
        # Tracing of:
        # * Total heat loss of a pipe per day
        Q_regime[[i]] <- Q_value <- loss[[i]] * len[[i]] * DAY  # [kcal]

        # * Specific heat loss power - magnitudes comparable with Minenergo-325
        #   sources
        loss_regime[[i]] <- loss_value <- loss[[i]]  # [kcal/m/h]

        # * Heat flux
        flux_regime[[i]] <- pipenostics::flux_loss(loss_value, d[[i]])  # [W/m²]

        # * Flow rate
        g_value <- {
          g_value +
          c(0, -g[[i]])[[forward + 1]] * c(g_value, 1)[[1 + absg]]  # [ton/h]
        }

        # * Temperature
        t_regime[[i]] <- t_value <- {
          t_value + pow(-1, forward) * pipenostics::dropt(
            temperature = t_value, pressure = p_value, flow_rate = g_value,
            loss_power = Q_value / DAY
          )  # [°C]
        }

        # * Pressure
        p_regime[[i]] <- p_value <- {
          p_value + pow(-1, forward) *
            pipenostics::dropp(
              temperature = t_value, pressure = p_value, flow_rate = g_value,
              d = (d[[i]] - 2 * wth[[i]]) * METER, len = len[[i]],
              roughness = roughness[[i]], inlet = inlet[[i]],
              outlet = outlet[[i]], method = method
            )
        }  # [MPa]

        # * Flow rate
        g_regime[[i]] <- g_value <- {
          g_value +
          c(0, g[[i]])[[2 - forward]] * c(g_value, 1)[[1 + absg]]  # [ton/h]
        }

      }
      list(
        temperature = t_regime, pressure = p_regime, flow_rate = g_regime,
        loss = loss_regime, flux = flux_regime, Q = Q_regime
      )
    }
  )
}
