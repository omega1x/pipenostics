#' @title
#'  Probability of failure of the corroded pipe
#'
#' @description
#'  Calculate \emph{probability of failure} of the corroded pipe taking into
#'  account its actual level of defectiveness and exploiting
#'  \href{https://en.wikipedia.org/wiki/Monte_Carlo_method#Monte_Carlo_and_random_numbers}{Monte-Carlo simulation}.
#'
#'  In the method they consider two possible failures for a single pipeline
#'  cross section with the on-surface and longitudinally oriented defect of the
#'  \emph{metal-loss} type:
#'
#'  \descibe{
#'     \item{rupture}{a decrease of the value of failure pressure down to the
#'     operating pressure}
#'     \item{leak}{increase of the corrosion depth (defect) up to the specified
#'     ultimate permissible fraction of pipe wall thickness.}
#'  }
#'
#'  They also assume change in depth and length of corrosion defects in time is
#'  close to linear.
#'
#'  @details
#'    Unlike
#'
#' @param depth
#'  maximum depth of the corroded area measured during \emph{inline inspection},
#'  [\emph{mm}]. Type: \code{[double]}.
#'
#' @param l
#'  maximum longitudinal length of corroded area measured during
#'  \emph{inline inspection}, [\emph{mm}]. Type: \code{[double]}.
#'
#' @param d
#'  nominal outside diameter of the pipe, [\emph{mm}]. Type: \code{[double]}.
#'
#' @param wth
#'  nominal wall thickness of the pipe, [\emph{mm}]. Type: \code{[double]}.
#'
#' @param strength
#'  one of the next characteristics of steel strength, [\emph{MPa}]:
#'  \itemize{
#'    \item specified minimum yield of stress (\emph{SMYS})
#'          for use with \code{\link{b31gpf}} and \code{\link{b31gmodpf}}.
#'    \item ultimate tensile strength (\emph{UTS}) or specified minimum tensile
#'          strength (\emph{SMTS}) for use with other failure pressure codes
#'          (\code{\link{dnvpf}}, \code{\link{pcorrcpf}}, \code{\link{shell92pf}}).
#'  }
#'  Type: \code{[double]}.
#'
#' @param pressure
#'  \href{https://en.wikipedia.org/wiki/Pressure_measurement#Absolute}{absolute pressure}
#'  of substance (i.e. heat carrier) inside the pipe measured near defect
#'  position, [\emph{MPa}]. In most cases this is a nominal operating pressure.
#'  Type: \code{[double]}.
#'
#' @param temperature
#'  temperature of substance (i.e. heat carrier) inside the pipe measured near
#'  defect position, [\emph{°C}]. In case of district heating network this is
#'  usually a calculated value according to actual or normative thermal-hydraulic
#'  regime. Type: \code{[double]}.
#'
#' @param rar
#'  random number generator for simulating of distribution of radial corrosion
#'  rate in pipe wall, [\emph{mm/day}]. Type: \code{[function]}. The only
#'  argument \code{n} of the function should be the number of observations to
#'  generate.
#'
#' @param ral
#'  random number generator for simulating of distribution of longitudinal corrosion
#'  rate in pipe wall, [\emph{mm/day}]. Type: \code{[function]}. The only
#'  argument \code{n} of the function should be the number of observations to
#'  generate.
#'
#' @param days
#'  number of days that have passed after or preceded the \emph{inline inspection}, [].
#'  Negative values are for retrospective assumptions whereas positives are for
#'  failure prognosis. Type: \code{[int]}.
#'
#' @param k
#'  alarm threshold for leakage failure. It usually \code{0.6}, \code{0.7}, or
#'  \code{0.8}, []. If set to \code{1} no alarm before failure occurs.
#'  Type: \code{[number]}.
#'
#' @param method
#'   method for calculating failure pressure:
#'   \itemize{
#'     \item \emph{b31g} - using \code{\link{b31gpf}}.
#'     \item \emph{b31gmod} - using \code{\link{b31gmodpf}}.
#'     \item \emph{dnv} - using \code{\link{dnvpf}}.
#'     \item \emph{pcorrc} - using \code{\link{pcorrcpf}}.
#'     \item \emph{shell92} - using \code{\link{shell92pf}}.
#'   }
#'   Type: \code{[choice]}.
#'
#' @param n
#'   number of observations to generate for
#'   \href{https://en.wikipedia.org/wiki/Monte_Carlo_method#Monte_Carlo_and_random_numbers}{Monte-Carlo simulations},
#'   Type: \code{[count]}.
#'
#'
#' @return
#'   Probability of pipe failure for each corroded area measured during
#'   \emph{inline inspection}. Type: \code{[double]}.
#'
#' @export
#'
#' @examples
pof <- function(depth = seq(0, 10, length.out = 100),
                l = seq(40, 50, length.out = 100),
                d = rep(762, 100),
                wth = rep(10, 100),
                strength = rep(358.5274, 100),
                pressure = rep(.588, 100),
                temperature = rep(150, 100),

                rar = function(n) runif(n, .01, .30) / 365,
                ral = function(n) runif(n, .01, .30) / 365,
                days = 0,
                k = .8,
                method = "b31g",
                n = 1e6
){

  # Checkmates ----
  # * values ====
  checkmate::assert_double(
    depth,
    lower = 0, upper = 1e3, finite = TRUE, any.missing = FALSE, min.len = 1
  )
  n_case <- length(depth)
  checkmate::assert_double(
    l,
    lower = 0, upper = 5e3,finite = TRUE, any.missing = FALSE, len = n_case
  )
  checkmate::assert_double(
    d,
    lower = 1, upper = 5e3, finite = TRUE, any.missing = FALSE, len = n_case
  )
  checkmate::assert_double(
    wth,
    lower = 0, upper = 5e2, finite = TRUE, any.missing = FALSE, len = n_case
  )
  checkmate::assert_double(
    strength,
    lower = 5, upper = 2e3, finite = TRUE, any.missing = FALSE, len = n_case
  )
  checkmate::assert_double(
    pressure,
    lower = 0, upper = 15, finite = TRUE, any.missing = FALSE, len = n_case
  )
  checkmate::assert_double(
    temperature,
    lower = 0, upper = 350, finite = TRUE, any.missing = FALSE, len = n_case
  )
  checkmate::assert_function(rar, args = "n", nargs = 1, null.ok = FALSE)
  ar_set <- rar(n)
  checkmate::assert_double(
    ar_set,
    lower = 2.7e-5, upper = 2.7e-3, any.missing = FALSE, len = n
  )
  checkmate::assert_function(ral, args = "n", nargs = 1, null.ok = FALSE)
  al_set <- ral(n)
  checkmate::assert_double(
    al_set,
    lower = 2.7e-5, upper = 2.7e-3, any.missing = FALSE, len = n)
  checkmate::assert_int(days)
  checkmate::assert_number(k, lower = .5, upper = 1.0, finite = TRUE)
  checkmate::assert_choice(
    method,
    c("b31g", "b31gmod", "dnv", "pcorrc", "shell92")
  )
  checkmate::assert_count(n, positive = TRUE)

  # * aspects ====
  checkmate::assert_true(all(wth >= .009*d & wth <= .15*d))
  checkmate::assert_true(all(depth <= wth))
  checkmate::assert_true(n > 1e6 - 1)

  mcplayer <- function(i){
    # Message ----
    cli_message <- paste(
      "\rpipenostics::pof: process case [%i/%i] - %i %%",
      c("processed.", ". All done, thanks!\n")
    )
    cat(sprintf(cli_message[1 + (i == n_case)], i, n_case, round(100*i)/n_case))

    # Models for PDFs ----
    # * Defect depth, [mm] ====
    u <- .1*(wth[[i]] - depth[[i]])
    depth_set <- runif(
      n,
      max(0, depth[[i]] - u), min(wth[[i]], depth[[i]] + u)
    ) + ar_set*days
    depth_set[depth_set < 0] <- 0
    depth_set[depth_set > wth[[i]]] <- wth[[i]]
    rm(u)

    # * Defect length, [mm] ====
    l_set <- runif(n, .95*l[[i]], min(5e3, 1.05*l[[i]])) + al_set*days
    l_set[l_set < 0] <- 0

    # * Pipe diameter, [mm] ====
    d_set <- runif(n, max(1, 0.9994*d[[i]]), min(5e3, 1.0006*d[[i]]))

    # * Wall thickness, [mm] ====
    wth_set <- runif(n, .967*wth[[i]], min(5e2, 1.033*wth[[i]]))

    # * Thermal-hydraulic regime: operational pressure, [MPa] ====
    p_set <- runif(n, .994*pressure[[i]], 1.006*pressure[[i]])  # sensor uncertainty

    # * Thermal-hydraulic regime: temperature, [C] ====
    t_set <- runif(n, max(0, temperature[[i]] - 2), min(350, temperature[[i]] + 2)) # sensor uncert.

    # * SMYS/UTC, [MPa] ====
    strength_set <-
      runif(n, max(5, .967*strength[[i]]), min(2e3, 1.033*strength[[i]]))
    strength_set <- pipenostics::strderate(strength_set, t_set)
    strength_set[strength_set < 5] <- 5

    # Diagnostic feature ----
    # * Failure pressure, [MPa] ====
    pf_set <- switch(method,
                     b31g = {
                       pf <- pipenostics::b31gpf(
                         pipenostics::inch_mm(d_set),
                         pipenostics::inch_mm(wth_set),
                         pipenostics::psi_mpa(strength_set),
                         pipenostics::inch_mm(depth_set),
                         pipenostics::inch_mm(l_set)
                       )
                       is_magnitude <- !is.na(pf)
                       pf[is_magnitude] <- pipenostics::mpa_psi(pf[is_magnitude])
                       pf
                     },

                     b31gmod = {
                       pf <- pipenostics::b31gmodpf(
                         pipenostics::inch_mm(d_set),
                         pipenostics::inch_mm(wth_set),
                         pipenostics::psi_mpa(strength_set),
                         pipenostics::inch_mm(depth_set),
                         pipenostics::inch_mm(l_set)
                       )
                       is_magnitude <- !is.na(pf)
                       pf[is_magnitude] <- pipenostics::mpa_psi(pf[is_magnitude])
                       pf
                     },
                     dnv = pipenostics::dnvpf(d_set, wth_set, strength_set, depth_set, l_set),
                     pcorrc = pipenostics::pcorrcpf(d_set, wth_set, strength_set, depth_set, l_set),
                     shell92 = pipenostics::shell92pf(d_set, wth_set, strength_set, depth_set, l_set)
    )


    # Probability of failure ----
    sum(
      p_set > (pf_set - .Machine$double.eps) |
        depth_set > (k*wth_set - .Machine$double.eps)
    )/n
  }
  # Loop over segments in cluster:
  vapply(seq_len(n_case), mcplayer, .1, USE.NAMES = FALSE)
}

# Bibliography
#[1] T. Sotberg, B.J. Leira, Reliability-based pipeline design and code calibration, in Proceedings
#of the 13th International Conference on Offshore Mechanics and Arctic Engineering, vol. V
#(1994), pp. 351–363
#
#[2] G.Jiao,T.Sotberg,R.Bruschi,R.Igland,The superb project: linepipe statistical properties and
# implications in design of offshore pipelines,in Proceedings of OMAE International Conference.
# ASME, vol. V (Yokohama, Japan, 1997)

#[3]  G. Jiao, T. Sotberg, R. Igland, SUPERB 2M statistical data—basic uncertainty measures for
#  reliability analysis of offshore pipelines. Report STF70 F95212. SINTEF, Trondheim, Norway
#  (1995)
