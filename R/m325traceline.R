#' @title
#'  Minenergo-325. Trace thermal-hydraulic regime for linear segment of district
#'  heating network
#'
#' @family Regime tracing
#'
#' @description
#'  Trace values of thermal-hydraulic regime (temperature, pressure,
#'  flow_rate, and other) along the adjacent linear segments of pipeline using
#'  norms of heat loss values prescribed by
#'  \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' @details
#'  They consider only simple tracing paths which do not contain rings and any
#'  kind of parallelization. At the same time bidirectional (forward and
#'  backward) tracing is possible in accordance with sensor position. They also
#'  may consider discharges to network at the inlet of each pipeline segment as
#'  an approximation of actual forks of flows.
#'
#'  Relevant illustration of the adopted assumptions for 4-segment tracing path
#'  is depicted on the next figure.
#'
#'  \figure{m325regtrace.png}
#'
#'  They make additional check for consistency of \code{inlet} and \code{outlet}
#'  values for subsequent pipe segments. Discrepancy of appropriate elevations
#'  cannot be more than \code{elev_tol}.
#'
#'  They also may consider some normative volume loss of heat carrier (water) by
#'  tuning volume loss factor \emph{a} in range \emph{0.0 -- 0.0025}
#'  \emph{h⁻¹}.
#'
#'  Optional verification of pipe diameters and wall thicknesses is performed
#'  against \code{\link{b36pipedata}} data.
#'
#' @param temperature
#'   \emph{Traced thermal hydraulic regime}. Sensor-measured temperature of heat
#'   carrier (water)
#'   inside the pipe sensor-measured at the inlet
#'   (forward tracing) or at the outlet (backward tracing) of path, [\emph{°C}].
#'   Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param pressure
#'   \emph{Traced thermal hydraulic regime}. Sensor-measured
#'   \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute
#'   pressure} of heat carrier (water) sensor-measured at the inlet
#'   (forward tracing) or at the outlet (backward tracing) of path,
#'   [\emph{MPa}].Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param flow_rate
#'   \emph{Traced thermal hydraulic regime}. Amount of heat carrier (water)
#'   sensor-measured at the inlet (forward tracing) or at the outlet (backward
#'   tracing) of path, [\emph{ton/h}].
#'   Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param g
#'   amount of heat carrier discharge to network for each pipe segment in the
#'   tracing path enumerated along the direction of flow. If flag \code{absg}
#'   is \code{TRUE} then they treat argument \code{g} as absolute value in
#'   [\emph{ton/h}], otherwise they do as percentage of flow_rate in the
#'   pipe segment.
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param a
#'   heat carrier (water) volume loss factor of cylindrical pipe,
#'   [\emph{h⁻¹}]. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param d
#'   nominal (outside) diameters of subsequent pipes in tracing path that are
#'   enumerated along the direction of flow, [\emph{mm}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'   nominal wall thickness of pipe, [\emph{mm}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param len
#'   length of subsequent pipes in tracing path that are enumerated
#'   along the direction of flow, [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param year
#'   year when pipe is put in operation after laying or total overhaul for
#'   each pipe in tracing path enumerated along the direction of flow.
#'   Type: \code{\link[checkmate]{assert_integerish}}.
#'
#' @param insulation
#'   insulation that covers the exterior of pipe:
#'   \describe{
#'     \item{\code{0}}{no insulation}
#'     \item{\code{1}}{foamed polyurethane or analogue}
#'     \item{\code{2}}{polymer concrete}
#'   }
#'   for each pipe in tracing path enumerated along the direction of flow.
#'   Type: \code{\link[checkmate]{assert_numeric}} and
#'   \code{\link[checkmate]{assert_subset}}.
#'
#' @param laying
#'   type of pipe laying depicting the position of pipe in space:
#'   \itemize{
#'     \item \code{air}
#'     \item \code{channel}
#'     \item \code{room}
#'     \item \code{tunnel}
#'     \item \code{underground}
#'   }
#'   for each pipe in tracing path enumerated along the direction of flow.
#'   Type: \code{\link[checkmate]{assert_character}} and
#'   \code{\link[checkmate]{assert_subset}}.
#'
#' @param beta
#'   logical indicator: should they consider additional heat loss of fittings?
#'   Logical value for each pipe in tracing path enumerated along the direction
#'   of flow. Type: \code{\link[checkmate]{assert_logical}}.
#'
#' @param exp5k
#'   logical indicator for regime of pipe: is pipe operated more that
#'   \code{5000} hours per year? Logical value for each pipe in tracing path
#'   enumerated along the direction of flow.
#'   Type: \code{\link[checkmate]{assert_logical}}.
#'
#' @param roughness
#'   roughness of internal wall for each pipe in tracing path enumerated along
#'   the direction of flow, [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param inlet
#'   elevation of pipe inlet for each pipe in tracing path enumerated along
#'   the direction of flow, [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param outlet
#'   elevation of pipe outlet for each pipe in tracing path enumerated along
#'   the direction of flow, [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param method
#'   method of determining \emph{Darcy friction factor}
#'   \itemize{
#'     \item \code{romeo}
#'     \item \code{vatankhah}
#'     \item \code{buzzelli}
#'   }
#'   Type: \code{\link[checkmate]{assert_choice}}. For more details see
#'   \code{\link{dropp}}.
#'
#' @param elev_tol
#'   maximum allowed discrepancy between adjacent outlet and inlet elevations of
#'   two subsequent pipes in the traced path, [\emph{m}].
#'   Type: \code{\link[checkmate]{assert_number}}.
#'
#' @param forward
#'   tracing direction flag: is it a forward direction of tracing?
#'   If \code{FALSE} the backward tracing is performed.
#'   Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @param absg
#'   whether argument \code{g} (amount of heat carrier discharge to network) is
#'   an absolute value in [\emph{ton/h}] (\code{TRUE}) or is it a percentage
#'   of flow rate in the pipe segment (\code{FALSE})?
#'   Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @param strict_sizes
#'   verify diameter and wall thickness with the actual pipe specifications
#'   produced. Type: \code{\link[checkmate]{assert_flag}}.
#'
#' @return
#'   A \code{\link{list}} containing results (detailed log) of tracing for each
#'   pipe in tracing path enumerated along the direction of flow:
#'   \describe{
#'     \item{\code{temperature}}{
#'       \emph{Traced thermal hydraulic regime}. Traced temperature of heat
#'        carrier (water), [\emph{°C}].
#'        Type: \code{\link[checkmate]{assert_double}}.
#'     }
#'     \item{\code{pressure}}{
#'       \emph{Traced thermal hydraulic regime}. Traced pressure of heat
#'        carrier (water) for each pipe in tracing path enumerated along the
#'        direction of flow, [\emph{MPa}].
#'        Type: \code{\link[checkmate]{assert_double}}.
#'     }
#'     \item{\code{flow_rate}}{
#'       \emph{Traced thermal hydraulic regime}. Traced flow rate of heat
#'        carrier (water) for each pipe in tracing path enumerated along the
#'        direction of flow, [\emph{ton/h}].
#'        Type: \code{\link[checkmate]{assert_double}}.
#'     }
#'     \item{\code{loss}}{
#'       \emph{Traced thermal hydraulic regime}. Normative specific heat
#'       loss power for each pipe in tracing path enumerated along the direction
#'       of flow, [\emph{kcal/m/h}].
#'       Type: \code{\link[checkmate]{assert_double}}.
#'     }
#'     \item{\code{flux}}{
#'       \emph{Traced thermal hydraulic regime}. Normative heat flux for each
#'       pipe in tracing path enumerated along the direction of flow,
#'       [\emph{W/m²}]. Type: \code{\link[checkmate]{assert_double}}.
#'     }
#'     \item{\code{Q}}{
#'       \emph{Traced thermal hydraulic regime}. Normative heat loss for each
#'       pipe in tracing path enumerated along the direction of flow per day,
#'       [\emph{kcal}]. Type: \code{\link[checkmate]{assert_double}}.
#'     }
#'   }
#'   Type: \code{\link[checkmate]{assert_list}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # Consider 4-segment tracing path depicted.
#' # First, let sensor readings for forward tracing:
#' t_fw <- 130  # [°C]
#' p_fw <- mpa_kgf(6) * all.equal(.588399, mpa_kgf(6))  # [MPa]
#' g_fw <- 250  # [ton/h]
#'
#' # Let discharges to network for each pipeline segment are somehow determined
#' # as
#' discharges <- seq(0, 30, 10)  # [ton/h]
#'
#' # Then the calculated regime (red squares) for forward tracing is
#' m325traceline(t_fw, p_fw, g_fw, discharges, forward = TRUE)
#'
#' # Next consider values of traced regime as sensor readings for backward
#' # tracing:
#' t_bw <- 127.3367  # [°C]
#' p_bw <- .5870330  # [MPa]
#' g_bw <- 190  # [ton/h]
#'
#' # Then the calculated regime (red squares) for backward tracing is
#' regime_bw <- m325traceline(t_bw, p_bw, g_bw, discharges, forward = FALSE)
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
#'       lambda(flow_rate[first],   g_fw)
#'     ),
#'     dimnames = list(
#'       c("temperature", "pressure", "flow_rate"),
#'       c("sensor.value", "traced.value", "abs.discr", "rel.discr")
#'     )
#'   )
#' })
#'
#' # To address the problem of possible norm losses of the heat carrier,
#' # they could roughly define the leaks as follows:
#'
#' # * maximum value prescribed to heat carrier loss factor, [h⁻¹],
#' #   multiplied by heating season relative duration:
#' a <- 0.0025 * ( (365 - 90)/365 )
#'
#' # * length of subsequent pipes, [m]:
#' l <- c(600, 530, 300, 350)
#'
#' # * nominal (outside) diameters of subsequent pipes, [m]:
#' D <- rep.int(700, 4) * 1e-3
#'
#' # * average year volumes of heat carrier (no heat carrier for 90 days in
#' #   summer), [m³]
#' V <- pi * D^2 / 4 * l
#'
#' # * finally they get,  [ton/h]:
#' discharges <- a * V * drop(iapws::if97("rho", mpa_kgf(6), k_c(130))) * 1e-3
#'
#' # * and the calculated regime (red squares) for forward tracing becomes
#' m325traceline(g = discharges, forward = TRUE)
#'
#' # Let's compare it with more formal calculations:
#' m325traceline(a = a, forward = TRUE)
#'
#' @export
m325traceline <- function(
  temperature = 130, pressure = mpa_kgf(6), flow_rate = 250,
  g = 0, # [ton/hr] or [] depending on
  a = 0,
  d = 711.0, wth = 12.70, # [mm]
  len = c(600, 530, 300, 350), # [m]
  year = 1986, insulation = 0, laying = "underground",
  beta = FALSE, exp5k = TRUE,
  roughness = 6e-3, inlet = 0., outlet = 0.,  elev_tol = .1,  # [m]
  method = "romeo",
  forward = TRUE, absg = TRUE, strict_sizes = FALSE
) {
  pow         <- .Primitive("^")
  DAY         <- 24 # [hours]
  NHL_N_POINT <- 2  # constan value for `extra` argument of m325nhl

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
    lower = 1e-3,   upper = 1e5, finite = TRUE
  )
  checkmate::assert_flag(absg)
  checkmate::assert_double(
    g,
    lower = 0, upper = c(1, flow_rate)[[1 + absg]], finite = TRUE,
    any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    a,
    lower = 0, upper = 25e-4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = min(pipenostics::m325nhldata[["d"]]),
    upper = max(pipenostics::m325nhldata[["d"]]),
    finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    wth,
    lower = min(pipenostics::b36pipedata[["wth"]]),
    upper = max(pipenostics::b36pipedata[["wth"]]),
    finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    len,
    lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_integerish(
    year,
    lower = 1900L, upper = max(pipenostics::m325nhldata[["epoch"]]),
    any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_subset(
    insulation,
    choices = unique(pipenostics::m325nhldata[["insulation"]])
  )
  checkmate::assert_subset(
    laying,
    choices = unique(pipenostics::m325nhldata[["laying"]]), empty.ok = FALSE
  )
  checkmate::assert_logical(beta,  any.missing = FALSE, min.len = 1L)
  checkmate::assert_logical(exp5k, any.missing = FALSE, min.len = 1L)
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
  checkmate::assert_number(elev_tol, lower = 0, upper = 10, finite = TRUE)
  checkmate::assert_choice(method, c("romeo", "vatankhah", "buzzelli"))
  checkmate::assert_flag(forward)
  checkmate::assert_flag(strict_sizes)

  checkmate::assert_true(commensurable(c(
    length(g), length(a), length(d), length(wth), length(len), length(year),
    length(insulation), length(laying), length(beta), length(exp5k),
    length(roughness), length(inlet), length(outlet)
  )
  ))
  checkmate::assert_true(all(d - 2 * wth > 0.5))  # in mm
  if (strict_sizes) checkmate::assert_true(all(pipenostics::b36dwthv(d, wth)))

  dh <- outlet - inlet
  checkmate::assert_true(all(abs(dh) < len))  # geometric constraint
  rm(dh)
  checkmate::assert_true(all(abs(
    utils::tail(inlet, -1) - utils::head(outlet, -1)
  ) < elev_tol))  # elevation consistency of tracing path

  if (forward) {
    # Material balance constraint:
    # *  approximately the utmost water density in this context, [ton/m³]:
    rho       <- 1.0

    mass_loss <- pipenostics::m325nvl(a, d, wth, len) * rho  # [ton/h]
    effective_flow <- flow_rate - sum(g, mass_loss)
    checkmate::assert_true(effective_flow >= 1e-3)
    rm(effective_flow, mass_loss, rho)
  }

  with(
    data.frame(
      g, a, d, wth, len, year, insulation, laying, beta, exp5k, roughness,
      inlet, outlet,

      # calculated regime parameters:
      t_regime = NA_real_, p_regime    = NA_real_,  g_regime    = NA_real_,
      Q_regime = NA_real_, loss_regime = NA_real_,  flux_regime = NA_real_
    ), {

      # declare temporary scalars for accumulation along the tracing path:
      t_value <- temperature
      p_value <- pressure
      g_value <- flow_rate
      Q_value <- loss_value <-  NA_real_

      pipe_enum <- with(list(enum = seq_along(g)), {
        if (forward) enum else rev(enum)
      })
      for (i in pipe_enum) {
        # Tracing of:
        # * Total heat loss of a pipe per day
        Q_regime[[i]] <- Q_value <- pipenostics::m325nhl(
          year = year[[i]], laying = laying[[i]], exp5k = exp5k[[i]],
          insulation = insulation[[i]], d = d[[i]], temperature = t_value,
          len = len[[i]], duration = DAY, beta = beta[[i]], extra = NHL_N_POINT
        )  # [kcal/day]

        # * Specific heat loss power - magnitudes comparable with Minenergo-325
        #   sources
        loss_regime[[i]] <- loss_value <- pipenostics::m325nhl(
          year = year[[i]],  laying = laying[[i]],  exp5k = exp5k[[i]],
          insulation = insulation[[i]],  d = d[[i]],  temperature = t_value,
          len = 1,  # [m]
          duration = 1,  # [h]
          beta = beta[[i]],  extra = NHL_N_POINT
        )  # [kcal/m/h]

        # * Heat flux
        flux_regime[[i]] <-
          pipenostics::flux_loss(loss_value, d[[i]])  # [W/m²]

        # * Heat carrier mass loss, [ton/h]:
        mass_loss <- pipenostics::m325nml(
          t_value, p_value, a[[i]], d[[i]], wth[[i]], len[[i]]
        )

        # * Flow rate in forward tracing, [ton/h]
        g_value <- {
          g_value + c(0, -g[[i]] - mass_loss)[[forward + 1]] *
          c(g_value, 1)[[1 + absg]]
        }

        # * Temperature, [°C]
        t_regime[[i]] <- t_value <- {
          t_value + pow(-1, forward) * pipenostics::dropt(
            temperature = t_value, pressure = p_value, flow_rate = g_value,
            loss_power = Q_value/DAY
          )
        }

        # * Pressure, MPa
        p_regime[[i]] <- p_value <- {
          p_value + pow(-1, forward) * pipenostics::dropp(
            temperature = t_value, pressure = p_value, flow_rate = g_value,
            d = (d[[i]] - 2 * wth[[i]]) * 1e-3,  # internal pipe diameter, [m]
            len = len[[i]], roughness = roughness[[i]], inlet = inlet[[i]],
            outlet = outlet[[i]], method = method
          )
        }

        # * Flow rate in backward tracing, [ton/h]
        g_regime[[i]] <- g_value <- {
          g_value +
          c(0, g[[i]] + mass_loss)[[2 - forward]] * c(g_value, 1)[[1 + absg]]
        }

      }
      list(
        temperature = t_regime,    pressure = p_regime,
        flow_rate   = g_regime,    loss     = loss_regime,
        flux        = flux_regime, Q        = Q_regime
      )
    }
  )
}
