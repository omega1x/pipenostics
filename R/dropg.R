#' @title
#'  Consumption drop in pipe
#'
#' @family district heating
#'
#' @description
#'  Calculate \emph{drop} or \emph{recovery} of consumption in pipe using
#'  geometric factors.
#'
#'  The calculated value may be positive or negative. When it is positive they
#'  have the \emph{drop}, i.e. the decrease of consumption in the outlet of pipe
#'  under consideration. When the calculated value is negative they have the
#'  \emph{recovery}, i.e. the increase of consumption in the outlet of pipe under
#'  consideration. In both cases to calculate consumption on the outlet of pipe
#'  under consideration simply subtract the calculated value from the
#'  sensor-measured consumption on the inlet.
#'
#' @param adj
#'  diameters of adjacent pipes through which discharges to and recharges from
#'  network occur, [\emph{mm}].
#'
#'  Types:
#'
#'  \describe{
#'    \item{\code{[double]}}{total diameter of all adjacent pipes (total diameter case)}
#'    \item{\code{[list]} of \code{[double]}}{a set of diameters of adjacent pipes (particular diameter case)}
#'  }
#'
#'  Positive values of diameters of adjacent pipes correspond to discharging
#'  process through those pipe, whereas negative values of diameters mean
#'  recharging. See \emph{Details} and \emph{Examples} for further explanations.
#'
#' @param d
#'   diameter of pipe under consideration, [\emph{mm}]. Type: \code{[double]}.
#'
#' @param consumption
#'   sensor-measured amount of heat carrier (water) that is transferred through
#'   the inlet of pipe during a period, [\emph{ton/hour}]. Type: \code{[double]}.
#'
#' @return
#'  consumption \emph{drop} or \emph{recovery} at the outlet of pipe,
#'  [\emph{ton/hour}], numeric vector. The value is positive for \emph{drop},
#'  whereas for \emph{recovery} it is negative. In both cases to calculate
#'  consumption on the outlet of pipe under consideration simply subtract the
#'  calculated value from the sensor-measured consumption on the inlet.
#'  Type: \code{[double]}.
#'
#' @details
#'  It is common that sensor-measured consumption undergoes discharges to
#'  network and recharges from it. For calculation of consumption \emph{drop} or
#'  \emph{recovery} the next configuration of district heating network segment is
#'  assumed:
#'
#'  \figure{dropg.png}
#'
#'  Usually, there are no additional sensors that could measure consumption in
#'  each flow fork. In that case they only may operate with geometric
#'  factors, i.e. assuming that flow rate is proportional to square of pipe
#'  diameter.
#'
#'  The simple summation of flow rates over all adjacent pipes produces
#'  the required consumption \emph{drop} or \emph{recovery} located on the
#'  outlet of the pipe under consideration. Since there is concurrency between
#'  discharges and recharges the diameters of discharge pipes are regarded
#'  positive whereas diameters of recharge pipes must be negative.
#'
#'  Be careful when dealing with geometric factors for large amount of recharges
#'  from network: there are no additional physical constraints and thus the
#'  calculated value of \emph{recovery} may have non-sense.
#'
#' @export
#'
#' @examples
#' # Let consider pipes according to network segment scheme depicted in figure
#' # in ?dropg help-page.
#'
#' # Typical large diameters of pipes under consideration, [mm]:
#' d <- as.double(unique(subset(pipenostics::m325nhldata, diameter > 700)$diameter))
#'
#' # Let sensor-measured consumption in the inlet of the pipe
#' # under consideration be proportional to d, [ton/hour]:
#' consumption <- .125*d
#'
#' # Let consider total diameter case when total diameters of adjacent pipes are no
#' # more than d, [mm]:
#' adj <- c(450, -400, 950, -255, 1152)
#'
#' # As at may be seen for the second and fourth cases they predominantly have
#' # recharges from network.
#' # Let calculate consumption on the outlet of the pipe under consideration,
#' # [ton/hour]
#'
#' result <- consumption - dropg(adj, d, consumption)
#' print(result)
#'
#' # [1]  75.96439 134.72222  65.70302 180.80580  78.05995
#'
#' # For more clarity they may perform calculations in data.table:
#' \dontrun{
#' dataset <- data.table::data.table(adj, d, consumption)
#' print(dataset)
#'
#' #     adj    d consumption
#' # 1:  450  800       100.0
#' # 2: -400  900       112.5
#' # 3:  950 1000       125.0
#' # 4: -255 1400       175.0
#' # 5: 1152 1200       150.0
#'
#' dataset[, drop := dropg(adj, d, consumption)]  # calculate drop and recovery
#' dataset[, result := consumption - drop]
#' print(dataset)
#'
#' #     adj    d consumption       drop    result
#' # 1:  450  800       100.0  24.035608  75.96439
#' # 2: -400  900       112.5 -22.222222 134.72222
#' # 3:  950 1000       125.0  59.296978  65.70302
#' # 4: -255 1400       175.0  -5.805804 180.80580
#' # 5: 1152 1200       150.0  71.940050  78.05995
#'}
#'
#' # Now let consider particular diameter case with the same total diameters of
#' # adjacent pipes, [mm]:
#'
#' adjp <- list(
#'   c(100, 175, 175, -65, 125, -60),  # diameters of 4 discharge pipes and 2 recharge pipes, [mm]
#'   c(-300, -100, -65, 125, -60),  # diameter of 1 discharge pipe and 4 recharge pipes, [mm]
#'   c(950),  # diameter of 1 discharge pipe, [mm]
#'   c(-255), # diameter of 1 recharge pipe, [mm]
#'   c(50, 70, 1000, 32)  # diameter of 4 discharge pipes, [mm]
#' )
#'
#' stopifnot(
#'   all(sapply(adjp, sum) == adj)
#' )
#'
#' # Recalculate the result:
#' result2 <- consumption - dropg(adjp, d, consumption)
#' stopifnot(
#'   all(result == result2)
#' )
#'
#' # They may do it in data.table:
#'\dontrun{
#' dataset <- data.table::data.table(adjp, d, consumption)
#' print(dataset)
#'
#' #                        adjp    d consumption
#' # 1:  100,175,175,-65,125,-60  800       100.0
#' # 2: -300,-100, -65, 125, -60  900       112.5
#' # 3:                      950 1000       125.0
#' # 4:                     -255 1400       175.0
#' # 5:        50,  70,1000,  32 1200       150.0
#'
#' dataset[, drop := dropg(adj, d, consumption)]  # calculate drop and recovery
#' dataset[, result := consumption - drop]
#' print(dataset)
#'
#' #                         adjp    d consumption       drop    result
#' # 1:  100,175,175,-65,125,-60  800       100.0  24.035608  75.96439
#' # 2: -300,-100, -65, 125, -60  900       112.5 -22.222222 134.72222
#' # 3:                      950 1000       125.0  59.296978  65.70302
#' # 4:                     -255 1400       175.0  -5.805804 180.80580
#' # 5:        50,  70,1000,  32 1200       150.0  71.940050  78.05995
#'}

dropg <- function(adj = 0, d = 700, consumption = 250) {
  UseMethod("dropg")
}

#' @export
dropg.list <- function(adj = 0, d = 700, consumption = 250){
  checkmate::assert_list(adj, types = "double", any.missing = FALSE, min.len = 1)
  checkmate::assert_double(d, lower = 25, upper = 2500, finite = TRUE,
                           any.missing = FALSE, min.len = 1)
  checkmate::assert_double(consumption, lower = 1e-3, upper = 1e5,
                           finite = TRUE, min.len = 1)
  adj <- sapply(adj, sum)
  NextMethod("dropg")
}

#' @export
dropg.default <- function(adj = 0, d = 700, consumption = 250){
  checkmate::assert_double(adj, finite = TRUE,
                           any.missing = FALSE, min.len = 1)
  checkmate::assert_double(d, lower = 25, upper = 2500, finite = TRUE,
                           any.missing = FALSE, min.len = 1)
  checkmate::assert_double(consumption, lower = 1e-3, upper = 1e5,
                           finite = TRUE, min.len = 1)
  sign(adj) * consumption * adj^2/(adj^2 * (adj > 0) + d^2)
}

