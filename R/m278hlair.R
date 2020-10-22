#' @title
#'  Minenergo-278. Heat losses of overhead pipeline segment
#'
#'
#' @family Minenergo
#'
#' @description
#'  Calculate values of heat flux emitted by overhead pipeline segment
#'  (surrounded by air) as a function of construction, operation,
#'  and technical condition  specifications according to
#'  Appendix 5.1 of \href{http://www.complexdoc.ru/ntdtext/547103/}{Minenergo Method 278}.
#'
#'  This type of calculations is usually made on design stage of district
#'  heating network (where water is a heat carrier) and is closely related to
#'  building codes and regulations.
#'
#' @param t1
#'   temperature of heat carrier (water) inside the supplying pipe, [\emph{°C}],
#'   numeric vector.
#' @param t2
#'   temperature of heat carrier (water) inside the returning pipe, [\emph{°C}],
#'   numeric vector.
#' @param t0
#'   temperature of environment, [\emph{°C}]. In case of overhead laying this is
#'   the ambient temperature, numeric vector.
#' @param insd1
#'   thickness of the insulator which covers the supplying pipe, [\emph{m}],
#'   numeric vector.
#' @param insd2
#'   thickness of the insulator which covers the returning pipe, [\emph{m}],
#'   numeric vector.
#' @param d1
#'   external diameter of supplying pipe, [\emph{m}], numeric vector.
#' @param d2
#'   external diameter of returning pipe, [\emph{m}], numeric vector.
#' @param lambda1
#'   thermal conductivity of insulator which covers the supplying pipe
#'   [\emph{W/m/°C}], numeric vector.
#' @param lambda2
#'   thermal conductivity of insulator which covers the returning pipe
#'   [\emph{W/m/°C}], numeric vector.
#' @param k1
#'   technical condition factor for insulator of supplying pipe, [], numeric vector.
#' @param k2
#'   technical condition factor for insulator of returning pipe, [], numeric vector.
#' @param lambda0
#'   thermal conductivity of environment, [\emph{W/m/°C}]. In case of overhead
#'   laying this is the thermal conductivity of open air, numeric vector.
#' @param len
#'  length of pipeline segment, [\emph{m}], numeric vector.
#' @param duration
#'  duration of heat flux emittance, [\emph{hour}], numeric vector.
#'
#' @return
#'  Heat flux emitted by pipeline segment during \code{duration}, [\emph{kcal}].
#'  If \code{len} of pipeline segment is 1 \emph{m} and \code{duration} of
#'  heat flux emittance is set to 1 \emph{hour} then the return value is equal
#'  to that in [\emph{kcal/m/h}] units and so comparable with values of
#'  heat flux listed in
#'  \href{http://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' @details
#'   Details on using \code{k1} and \code{k2} are the same as for
#'   \code{\link{m278hlcha}}.
#' @export
#'
#' @examples
#'  ## Dummy test:
#'  stopifnot(round(m278hlair(), 3) == 138.774)
#'
m278hlair <-
  function(t1 = 110,
           t2 = 60,
           t0 = 5,
           insd1 = 0.1,
           insd2 = insd1,
           d1 = .25,
           d2 = d1,
           lambda1 = 0.09,
           lambda2 = 0.07,
           k1 = 1,
           k2 = k1,
           lambda0 = 26,
           len = 1,
           duration = 1
           ) {
    checkmate::assert_double(
      t1,
      lower = 0,
      upper = 450,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      t2,
      lower = 0,
      upper = 450,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      t0,
      lower = -15,
      upper = 30,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      insd1,
      lower = 0,
      upper = .5,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      insd2,
      lower = 0,
      upper = .5,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      d1,
      lower = .2,
      upper = 1.5,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      d2,
      lower = .2,
      upper = 1.5,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      lambda1,
      lower = 1e-3,
      upper = 1,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      lambda2,
      lower = 1e-3,
      upper = 1,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      k1,
      lower = 1,
      upper = 4.5,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      k2,
      lower = 1,
      upper = 4.5,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(
      lambda0,
      lower = 1,
      upper = 100,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_double(len,
                             lower = 0,
                             finite = TRUE,
                             any.missing = FALSE)
    checkmate::assert_double(duration,
                             lower = 0,
                             finite = TRUE,
                             any.missing = FALSE)

    q1 <-
      pi * (t1 - t0) / (log((d1 + 2 * insd1) / d1) / (2 * k1 * lambda1) + 1 /
                          (lambda0 * (d1 + 2 * insd1)))
    q2 <-
      pi * (t2 - t0) / (log((d2 + 2 * insd2) / d2) / (2 * k2 * lambda2) + 1 /
                          (lambda0 * (d2 + 2 * insd2)))
    q <- q1 + q2
    q * len * duration

}