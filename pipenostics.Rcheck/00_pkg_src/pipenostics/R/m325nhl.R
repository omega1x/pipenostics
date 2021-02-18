#' @title
#'  Minenergo-325. Normative heat losses of pipe
#'
#' @family Minenergo
#'
#' @description
#'  Calculate normative values of heat flux that is legally affirmed by
#'  \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325} to be
#'  emitted by steel pipe of district heating system with water as a heat carrier.
#'
#' @param year
#'   year when the pipe is put in operation after laying or total overhaul.
#'   Type: \code{\link{assert_integerish}}
#'
#' @param laying
#'  type of pipe laying depicting the position of pipe in space:
#'  \itemize{
#'    \item \code{air},
#'    \item \code{channel},
#'    \item \code{room},
#'    \item \code{tunnel},
#'    \item \code{underground}.
#'  }
#'  Type: \code{\link{assert_subset}}.
#'
#' @param exp5k
#'  pipe regime flag: is pipe operated more that 5000 hours per year?
#'  Type: \code{\link{assert_logical}}.
#'
#' @param insulation
#'  insulation that covers the exterior of pipe:
#'  \describe{
#'    \item{\code{0}}{no insulation}
#'    \item{\code{1}}{foamed polyurethane or analogue}
#'    \item{\code{2}}{polymer concrete}
#'  }
#'  Type: \code{\link{assert_integer}} and \code{\link{assert_subset}}.
#'
#' @param d
#'   internal diameter of pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @param temperature
#'  temperature of heat carrier (water) inside the pipe, [\emph{°C}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param len
#'  length of pipe, [\emph{m}]. Type: \code{\link{assert_double}}.
#'
#' @param duration
#'  duration of heat flux emittance, [\emph{hour}]. Type: \code{\link{assert_double}}.
#'
#' @param beta
#'  should they consider additional heat losses of fittings?
#'  Type: \code{\link{assert_logical}}.
#'
#' @param extra
#'   number of points used for temperature extrapolation: \code{2}, \code{3},
#'   or \code{4}. Type: \code{\link{assert_choice}}.
#'
#' @return
#'  Heat flux emitted by pipe during \code{duration}, [\emph{kcal}].
#'  If \code{len} of pipe is 1 \emph{m} and \code{duration} of heat flux
#'  emittance is set to 1 \emph{hour} then the return value is in the same
#'  units as value of heat flux, [\emph{kcal/m/h}], accepted by
#'  \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'  Type: \code{\link{assert_double}}.
#'
#' @details
#'  Temperature extrapolation and pipe diameter interpolation are leveraged
#'  for better accuracy. Both are linear as it dictated by
#'  \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'  Nevertheless, one could control the extrapolation behavior by \code{extra}
#'  argument: use lower values of \code{extra} for soft curvature near extrapolation
#'  edges, and higher values for more physically reasoned behavior in far regions
#'  of extrapolation.
#'
#' @export
#'
#' @examples
#'
#'  with(m325nhldata, {
#'
#'  ## Linear extrapolation adopted in Minenergo's Order 325 using last two points:
#'  temperature <- seq(0, 270, 10)  # [°C]
#'  flux <- m325nhl(1980, "underground", TRUE, 0, 73, temperature)  # [kcal/m/h]
#'  plot(temperature, flux, type = "b")
#'
#'  ## Consider heat losses of fittings:
#'  stopifnot(
#'    ## when beta becomes 1.15
#'    all(
#'      round(
#'        m325nhl(1980, "underground", d = 73, temperature = 65,
#'                beta = c(FALSE, TRUE)),
#'        3
#'      ) == c(65.500, 75.325)
#'    ),
#'
#'    ## when beta becomes 1.2
#'    all(
#'      round(
#'        m325nhl(2000, "channel", d = 73, temperature = 65,
#'                beta = c(FALSE, TRUE)),
#'        3
#'      ) == c(17.533, 21.040)
#'    )
#'  )
#' })
#'
#'
m325nhl <- function(year = 1986, laying = "underground", exp5k = TRUE,
                    insulation = 0, d = 700, temperature = 110, len = 1,
                    duration = 1, beta = FALSE, extra = 2) {
  norms <- pipenostics::m325nhldata
  checkmate::assert_integerish(year, lower = 1900L,
                               upper = max(norms$epoch),
                               any.missing = FALSE,
                               min.len = 1)
  checkmate::assert_subset(laying, choices = unique(norms$laying),
                           empty.ok = FALSE)
  checkmate::assert_logical(exp5k, any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(insulation, choices = unique(norms$insulation))
  checkmate::assert_double(d, lower = min(norms$diameter),
                           upper = max(norms$diameter),
                           finite = TRUE, any.missing = FALSE,
                           min.len = 1)
  checkmate::assert_double(temperature, lower = 0, upper = max(norms$temperature),
                           finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(len, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_double(duration, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1)
  checkmate::assert_logical(beta, any.missing = FALSE, min.len = 1)
  checkmate::assert_choice(extra, 2:4)

  worker <-
    function(year_value, laying_value, exp5k_value, insulation_value,
             d_value, temperature_value, len_value, duration_value, beta_value) {
      epoch <- with(list(epochs = unique(norms$epoch)), {
        epochs[[findInterval(year_value, epochs, left.open = TRUE) + 1]]
      })
      norms <- norms[norms$epoch == epoch &
                     norms$laying == laying_value &
                     norms$exp5k == exp5k_value &
                     norms$insulation == insulation_value, ]
      neighbor_diameter <-
        with(list(norms_diameter = unique(norms$diameter)), {
          checkmate::assert_double(
            d_value, lower = min(norms_diameter), upper = max(norms_diameter),
            finite = TRUE, any.missing = FALSE, unique = TRUE)
          n <- findInterval(d_value, norms_diameter)
          norms_diameter[c(n, n + 1L)]
        })
      flux <- vapply(neighbor_diameter, function(x, t_carrier) {
        if (is.na(x)) return(NA_real_)
        with(norms[norms$diameter == x, c("temperature", "flux")], {
          zone <- findInterval(t_carrier, range(temperature),
                               rightmost.closed = TRUE)
          if (zone == 1L) {
            stats::approx(x = temperature, y = flux, xout = t_carrier)$y
          } else {
            j <- order(temperature, decreasing = as.logical(zone))[1:extra]
            drop(coef(lsfit(temperature[j], flux[j])) %*% c(1., t_carrier))
          }
        })
      }, FUN.VALUE = 1., t_carrier = temperature_value)
      flux <-
        if (all(!is.na(flux))) stats::approx(neighbor_diameter, flux, d_value)$y else
          flux[!is.na(flux)][[1L]]
      flux * len_value * duration_value * (
        pipenostics::m325beta(laying_value, d_value) * beta_value + !beta_value
      )
  }
  unlist(
    Map(
      worker, year, laying, exp5k, insulation, d, temperature, len, duration,
      beta
    )
  )
}




