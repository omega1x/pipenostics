#' @title
#'  Convert heat flux to specific heat loss power
#'
#' @family Measurement Unit Converter
#'
#' @description
#'  Convert \href{https://en.wikipedia.org/wiki/Heat_flux}{heat flux}
#'  measured for a cylindrical steel pipe to \emph{specific heat loss power} of
#'  pipe.
#'
#' @param x
#'  value of
#'  \itemize{
#'    \item \emph{heat flux}, [\emph{W/m²}], for \code{loss_flux(x, d, wth)}
#'    \item \emph{specific heat loss power}, [\emph{kcal/m/h}], for
#'    \code{flux_loss(x, d, wth)(x)}
#'  }
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param d
#'  nominal (outside) diameter of pipe, [\emph{mm}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  Value of
#'  \itemize{
#'    \item \emph{specific heat loss power}, [\emph{kcal/m/h}], for
#'          \code{loss_flux(x, d, wth)}
#'    \item \emph{heat flux}, [\emph{W/m²}], for \code{flux_loss(x, d, wth)(x)}
#'  }
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @examples
#' library(pipenostics)
#'
#' # Consider pipes with nominal (outside) diameters:
#' d <- c(998, 1395)  # [mm]
#'
#' # Then maximum possible normative neat loss according (Minenergo-325) is
#' loss_max <- c(218, 1040)  # [kcal/m/h]
#'
#' # The appropriate flux is
#' flux <- flux_loss(loss_max, d)
#' print(flux)
#'
#' stopifnot(all.equal(loss_flux(flux, d), loss_max, tolerance = 1e-5))
#'
#' @rdname flux
#' @export
loss_flux <- function(x, d = 720) {
  checkmate::assert_double(
    x,
    lower = 0, upper = 6e4, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = 1, upper = 5e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  commensurable(c(length(x), length(d)))

  h <- 3600    # [s/h]
  J <- 4186.8  # [J/kcal]
  m <- 1e-3    # [m/mm]  #x <- flux  # [W/m²]

  x * h / J * base::pi * d * m  # [kcal/m/h]
}

#' @rdname flux
#' @export
flux_loss <- function(x, d = 720) {
  checkmate::assert_double(
    x,
    lower = 0, upper = 1500, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = 1, upper = 5e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  commensurable(c(length(x), length(d)))

  h <- 3600    # [s/h]
  J <- 4186.8  # [J/kcal]
  m <- 1e-3    # [m/mm] #x <- loss    # [kcal/m/h]

  x * J / h / base::pi / d / m  # [W/m²]
}
