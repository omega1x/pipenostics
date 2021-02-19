#' @title
#'  Minenergo-325. Trace thermal-hydraulic regime for linear segment
#'
#' @family Regime tracing
#'
#' @description
#'  Trace values of thermal-hydraulic regime (temperature, pressure,
#'  consumption) along the adjacent linear segments of pipeline using norms of
#'  heat flux values prescribed by
#'  \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' @details
#'  The calculated (values of) regime may be considered as representation of
#'  district heating process in conditions of hypothetically perfect
#'  technical state of pipe walls and insulation.
#'
#'  They consider only simple tracing paths which do not contain rings and any
#'  kind of parallelization. At the same time bidirectional (forward and
#'  backward) tracing is possible in accordance with sensor position. They also
#'  may consider discharges to network at the inlet of each pipeline segment
#'  as an approximation of actual forks of flows. Relevant illustration of
#'  adopted assumptions for 4-segment tracing path is depicted on the next
#'  figure.
#'
#'  \figure{m325regtrace.png}
#'
#'  They make additional check for consistency of \code{inlet} and \code{outlet}
#'  values for subsequent pipe segments. Discrepancy of appropriate elevations
#'  cannot be more than \code{elev_tol}.
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe sensor-measured at the inlet
#'  (forward tracing) or at the outlet (backward tracing) of path, [\emph{°C}].
#'  Type: \code{\link{assert_number}}.
#'
#' @param pressure
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'  of heat carrier (water) sensor-measured at the inlet
#'  (forward tracing) or at the outlet (backward tracing) of path, [\emph{MPa}].
#'  Type: \code{\link{assert_number}}.
#'
#' @param consumption
#'  amount of heat carrier (water) sensor-measured at the inlet (forward tracing) or at
#'  the outlet (backward tracing) of path, [\emph{ton/hour}].
#'  Type: \code{\link{assert_number}}.
#'
#' @param g
#'  amount of heat carrier discharge to network for each pipe segment in the
#'  tracing path enumerated along the direction of flow. If flag \code{absg}
#'  is \code{TRUE} then they treat argument \code{g} as absolute value in
#'  [\emph{ton/hour}], otherwise they do as percentage of consumption in the
#'  pipe segment.
#'  Type: \code{\link{assert_double}}.
#'
#' @param d
#'  internal diameters of subsequent pipes in tracing path that are enumerated
#'  along the direction of flow, [\emph{mm}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param len
#'  length of subsequent pipes in tracing path that are enumerated
#'  along the direction of flow, [\emph{m}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param year
#'   year when pipe is put in operation after laying or total overhaul for
#'   each pipe in tracing path enumerated along the direction of flow.
#'   Type: \code{\link{assert_integerish}}.
#'
#' @param insulation
#'  insulation that covers the exterior of pipe:
#'  \describe{
#'    \item{\code{0}}{no insulation}
#'    \item{\code{1}}{foamed polyurethane or analogue}
#'    \item{\code{2}}{polymer concrete}
#'  }
#'  for each pipe in tracing path enumerated along the direction of flow.
#'  Type: \code{\link{assert_numeric}} and \code{\link{assert_subset}}.
#'
#' @param laying
#'  type of pipe laying depicting the position of pipe in space:
#'  \itemize{
#'    \item \code{air}
#'    \item \code{channel}
#'    \item \code{room}
#'    \item \code{tunnel}
#'    \item \code{underground}
#'  }
#'  for each pipe in tracing path enumerated along the direction of flow.
#'  Type: \code{\link{assert_character}} and \code{\link{assert_subset}}.
#'
#' @param beta
#'  should they consider additional heat losses of fittings? Logical value
#'  for each pipe in tracing path enumerated along the direction of flow.
#'  Type: \code{\link{assert_logical}}.
#'
#' @param exp5k
#'  pipe regime flag: is pipe operated more that \code{5000} hours per year? Logical
#'  value for each pipe in tracing path enumerated along the direction of flow.
#'  Type: \code{\link{assert_logical}}.
#'
#' @param roughness
#'  roughness of internal wall for each pipe in tracing path enumerated along
#'  the direction of flow, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param inlet
#'  elevation of pipe inlet for each pipe in tracing path enumerated along
#'  the direction of flow, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param outlet
#'  elevation of pipe outlet for each pipe in tracing path enumerated along
#'  the direction of flow, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param method
#'  method of determining \emph{Darcy friction factor}
#'  \itemize{
#'    \item \code{romeo}
#'    \item \code{vatankhan}
#'    \item \code{buzelli}
#'  }
#'  Type: \code{\link{assert_choice}}. For more details see \code{\link{dropp}}.
#'
#' @param elev_tol
#'  maximum allowed discrepancy between adjacent outlet and inlet elevations of
#'  two subsequent pipes in the traced path, [\emph{m}].
#'  Type: \code{\link{assert_number}}.
#'
#' @param forward
#'  tracing direction flag: is it a forward direction of tracing?
#'  If \code{FALSE} the backward tracing is performed.
#'  Type: \code{\link{assert_flag}}.
#'
#' @param absg
#'  Whether argument \code{g} (amount of heat carrier discharge to network) is an
#'  absolute value in [\emph{ton/hour}] (\code{TRUE}) or is it a percentage of
#'  consumption in the pipe segment (\code{FALSE})?
#'  Type: \code{\link{assert_flag}}.
#'
#' @return
#'   named list of regime parameters for the traced path with the next elements:
#'  \describe{
#'    \item{\code{temperature}}{calculated temperatures of heat carrier for all pipeline segments, [\emph{°C}].
#'    Type: \code{\link{assert_double}}.}
#'    \item{\code{pressure}}{calculated pressures of heat carrier for all pipeline segments, [\emph{MPa}].
#'    Type: \code{\link{assert_double}}.}
#'    \item{\code{consumption}}{calculated consumption(s) of heat carrier for all pipeline segments, [\emph{ton/hour}].
#'    Type: \code{\link{assert_double}}.}
#'  }
#'
#' @seealso
#'  \code{\link{m325dropt}} for calculating normative temperature drop in
#'  single pipeline segment
#'
#' @examples
#' # Consider 4-segment tracing path depicted in ?m325regtrace help page.
#' # First, let sensor readings for forward tracing:
#' t_fw <- 130  # [°C]
#' p_fw <- .588399*all.equal(.588399, mpa_kgf(6))  # [MPa]
#' g_fw <- 250  # [ton/hour]
#'
#' # Let discharges to network for each pipeline segment are somehow determined as
#' discharges <- seq(0, 30, 10)  # [ton/hour]
#'
#' \donttest{
#' # Then the calculated regime (red squares) for forward tracing is
#' regime_fw <- m325traceline(t_fw, p_fw, g_fw, discharges, forward = TRUE)
#' print(regime_fw)
#'
#' # $temperature
#' # [1] 129.1799 128.4269 127.9628 127.3367
#' #
#' # $pressure
#' # [1] 0.5878607 0.5874226 0.5872143 0.5870330
#' #
#' # $consumption
#' # [1] 250 240 220 190
#'}
#' # Next consider values of traced regime as sensor readings for backward tracing:
#' t_bw <- 127.3367  # [°C]
#' p_bw <- .5870330  # [MPa]
#' g_bw <- 190  # [ton/hour]
#'
#' # Then the calculated regime (red squares) for backward tracing is
#' \donttest{
#' regime_bw <- m325traceline(t_bw, p_bw, g_bw, discharges, forward = FALSE)
#' print(regime_bw)
#'
#' # $temperature
#' # [1] 129.9953 129.1769 128.4254 127.9619
#' #
#' # $pressure
#' # [1] 0.5883998 0.5878611 0.5874228 0.5872144
#' #
#' # $consumption
#' # [1] 250 250 240 220
#'
#' # Let compare sensor readings with backward tracing results:
#' tracing <- with(regime_bw, {
#'   lambda <- function(val, constraint)
#'     c(val, constraint, constraint - val,
#'       abs(constraint - val)*100/constraint)
#'   first <- 1
#'   structure(
#'     rbind(
#'       lambda(temperature[first], t_fw),
#'       lambda(pressure[first],    p_fw),
#'       lambda(consumption[first], g_fw)
#'     ),
#'     dimnames = list(
#'       c("temperature", "pressure", "consumption"),
#'       c("sensor.value", "traced.value", "abs.discr", "rel.discr")
#'     )
#'   )
#' })
#' print(tracing)
#'
#' # sensor.value traced.value     abs.discr    rel.discr
#' # temperature   130.000000  129.9952943  4.705723e-03 0.0036197868
#' # pressure        0.588399    0.5883998 -8.290938e-07 0.0001409067
#' # consumption   250.000000  250.0000000  0.000000e+00 0.0000000000
#'}
#' @export
m325traceline <- function(
  temperature = 130, pressure = mpa_kgf(6), consumption = 250,
  g = 0,  # [ton/hour] or [] depending on
  d = 700,  # [mm]
  len = c(600, 530, 300, 350),  # [m]
  year = 1986,
  insulation = 0,
  laying = "underground",
  beta = FALSE,
  exp5k = TRUE,
  roughness = 6e-3,  # [m]
  inlet = 0., outlet = 0.,  # [m]
  elev_tol = .1,  # [m]
  method = "romeo",
  forward = TRUE,
  absg = TRUE
  ){
  norms <- pipenostics::m325nhldata
  checkmate::assert_number(temperature, lower = 0, upper = 350, finite = TRUE)
  checkmate::assert_number(pressure, lower = 8.4e-2, upper = 100, finite = TRUE)
  checkmate::assert_number(consumption, lower = 1e-3, upper = 1e5, finite = TRUE)
  checkmate::assert_flag(absg)
  checkmate::assert_double(g, lower = 0,
                           upper = c(1, consumption)[[1 + absg]],
                           finite = TRUE, any.missing = FALSE,
                           min.len = 1)
  checkmate::assert_double(d, lower = min(norms$diameter),
                           upper = max(norms$diameter),
                           finite = TRUE, any.missing = FALSE,
                           min.len = 1)
  checkmate::assert_double(len, lower = 0, finite = TRUE, any.missing = FALSE,
                           min.len = 1)
  checkmate::assert_integerish(year, lower = 1900L,
                               upper = max(norms$epoch),
                               any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(insulation, choices = unique(norms$insulation))
  checkmate::assert_subset(laying, choices = unique(norms$laying),
                           empty.ok = FALSE)
  checkmate::assert_logical(beta, any.missing = FALSE, min.len = 1)
  checkmate::assert_logical(exp5k, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(roughness, lower = 0, upper = .2, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(inlet, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(outlet, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_number(elev_tol, lower = 0, upper = 10, finite = TRUE)
  checkmate::assert_choice(method, c("romeo", "vatankhan", "buzelli"))
  checkmate::assert_flag(forward)

  dh <- outlet - inlet
  checkmate::assert_true(all(abs(dh) < len))  # geometric constraint
  checkmate::assert_true(
    all(abs(utils::tail(inlet, -1) - utils::head(outlet, -1)) < elev_tol)
  )  # elevation consistency of tracing path

  # material balance constraint
  if (forward) checkmate::assert_true(consumption - sum(g) >= 1e-3)

  with(data.frame(g, d, len, year, insulation, laying, beta, exp5k,
                  roughness, inlet, outlet,
                  # calculated regime parameters:
                  t_regime = NA_real_, p_regime = NA_real_, g_regime = NA_real_
                  ), {
    # declare temporary scalars for accumulation along the tracing path:
    t_value <- temperature; p_value <- pressure; g_value <- consumption
    pipe_enum <- with(list(enum = seq_len(length(g))), {
      if (forward) enum else rev(enum)
    })
    for (i in pipe_enum) {
      g_value <- g_value + c(0, -g[[i]])[[forward + 1]]*c(g_value, 1)[[1 + absg]]
      t_regime[[i]] <- t_value <- {
        t_value + (-1)^forward *
        pipenostics::m325dropt(
          temperature = t_value, pressure = p_value, consumption = g_value,
          d = d[[i]], len = len[[i]], year = year[[i]],
          insulation = insulation[[i]], laying = laying[[i]], beta = beta[[i]],
          exp5k = exp5k[[i]]
        )
      }
      p_regime[[i]] <- p_value <- {
        p_value + (-1)^forward *
        pipenostics::dropp(
          temperature = t_value, pressure = p_value, consumption = g_value,
          d = d[[i]]*1e-3, len = len[[i]], roughness = roughness[[i]],
          inlet = inlet[[i]], outlet = outlet[[i]], method = method
        )
      }
      g_regime[[i]] <- g_value <-
        g_value + c(0, g[[i]])[[2 - forward]]*c(g_value, 1)[[1 + absg]]
    }
  list(temperature = t_regime, pressure = p_regime, consumption = g_regime)
  })
}
