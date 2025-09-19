#' @title
#'  ASME B31G. Basic computer program CRVL.BAS
#'
#' @family ASME B31G
#'
#' @description
#'  Imitation of \emph{CVRL.BAS} computer program presented in
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{
#'  ASME B31G-1991} \emph{Appendix A} for determining allowable length and
#'  allowable operating pressure
#'
#' @param maop
#'  maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param smys
#'  specified minimum yield of stress (\emph{SMYS}) as a characteristics of
#'  steel strength, [\emph{PSI}]. Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param def
#'  appropriate (combined) design factor from
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.4.2002.pdf}{
#'  ASME B31.4},
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.8.2003.pdf}{
#'  ASME B31.8}, or
# nolint start: line_length_linter.
#'  \href{https://www.asme.org/codes-standards/find-codes-standards/b31-11-slurry-transportation-piping-systems}{
# nolint end
#'   ASME B31.11}, [].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param depth
#'   measured maximum depth of the corroded area, [\emph{inch}].
#'   Type: \code{\link[checkmate]{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of the corroded area, [\emph{inch}].
#'  Type: \code{\link[checkmate]{assert_double}}.
#'
#' @return
#'  Object of \emph{S3}-class \emph{crvl} which is a \emph{data.frame} with
#'  the next numeric columns:
#'
#'  \describe{
#'    \item{maop}{
#'      maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{d}{
#'      nominal outside diameter of pipe, [\emph{inch}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{wth}{
#'      nominal wall thickness of pipe, [\emph{inch}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{smys}{
#'      specified minimum yield of stress (\emph{SMYS}) as a characteristics of
#'      steel strength, [\emph{PSI}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'     }
#'    \item{def}{
#'      appropriate (combined) design factor from
#'      \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.4.2002.pdf}{
#'      ASME B31.4},
#'      \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.8.2003.pdf}{
#'      ASME B31.8},
#'      or
# nolint start: line_length_linter.
#'      \href{https://www.asme.org/codes-standards/find-codes-standards/b31-11-slurry-transportation-piping-systems}{ASME B31.11},
# nolint end
#'      []. Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{depth}{
#'      measured maximum depth of the corroded area, [\emph{inch}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{l}{
#'      measured maximum longitudial length of corroded area, [\emph{inch}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{status}{
#'    operational status of pipe:
#'      \emph{1} - excellent,
#'      \emph{2} - monitoring is recommended,
#'      \emph{3} - alert! replace the pipe immediately!
#'      Type: \code{\link[checkmate]{assert_numeric}}.
#'    }
#'    \item{design_pressure}{
#'      design pressure of pipe, [\emph{PSI}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{safe_pressure}{
#'      safe maximum pressure for the corroded area, [\emph{PSI}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{pressure_exceeding}{
#'      whether operator's action is required to reduce \emph{MOAP} lower than
#'      the maximum safe pressure of the corroded area.
#'      Type: \code{\link[checkmate]{assert_logical}}.
#'    }
#'    \item{allowed_corrosion_depth}{
#'      allowable depth of the corroded area, [\emph{inch}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{A}{
#'      intermediate factor related to the geometry of the corroded area, [].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{allowed_corrosion_length}{
#'      allowable length of the corroded area, [\emph{inch}].
#'      Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'    \item{AP}{
#'      another intermediate factor related to the geometry of the corroded
#'      area, []. Type: \code{\link[checkmate]{assert_double}}.
#'    }
#'  }
#'
#' @details
#'   Columns \emph{maop}, \emph{d}, \emph{wth}, \emph{smys}, \emph{def},
#'   \emph{depth}, \emph{l} in the output \emph{data.frame} come from
#'   function's input, other columns are calculated.
#'
#'   For univariate case (when lengths of all input vectors are one) messages
#'   that imitate \emph{CRVL.BAS} console output are printed.
#'
#' @references
#'  \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{
#'  ASME B31 G-1991}.
#'  Manual for determining the remaining strength of corroded pipelines. A
#'  supplement to \emph{ASME B31G} code for pressure piping.
#'
#' @export
#'
#' @examples
#' library(pipenostics)
#'
#' ## Further examples are inspired by those used in Appendix A of
#' ## ASME B31G-1991 to verify correct entry of CRVL.BAS source code:
#'
#' ## Standard example 1
#' b31crvl(
#'   maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1,
#'   l = 7.5
#' )
#'
#' ## Standard example 2
#' b31crvl(
#'   maop = 400, d = 20, wth = .25, smys = 35000, def  = 0.5, depth = 0.18,
#'   l = 10
#' )
#'
#' ## Standard example 3
#' b31crvl(
#'   maop = 910, d = 24, wth = .432, smys = 52000, def  = .72, depth = 0.13,
#'   l = 30
#' )
#'
#' ## Standard example 4
#' b31crvl(
#'   maop = 910, d = 24, wth = .432, smys = 52000, def  = .72, depth = .3,
#'   l = 30
#' )
#'
#' ## Standard example 5
#' b31crvl(
#'   maop = 731, d = 24, wth = .281, smys = 52000, def  = 0.72, depth = 0.08,
#'   l = 15
#' )
#'
#' ## Standard example 6
#' b31crvl(
#'   maop = 1e3, d = 36, wth = .5, smys = 52000, def  = 0.72, depth = 0.41,
#'   l = 100
#' )
#'
#' ## Standard example 8
#' b31crvl(
#'   maop = 877, d = 12.625, wth = .5, smys = 35000, def  = .4, depth = .035,
#'   l = 3
#' )
#'
#' ## Standard example 9
#' b31crvl(
#'   maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .125, l = 12
#' )
#'
#' ## TEST #1
#' b31crvl(
#'   maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179, l = 12
#' )
#'
#' ## TEST #1A
#' b31crvl(
#'   maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179,
#'   l = 12.182
#' )
#'
#' ## TEST #1B
#' b31crvl(
#'   maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .180,
#'   l = 12.182
#' )
#'
#' ## TEST #2
#' b31crvl(
#'   maop = 790, d = 24, wth = .5, smys = 42000, def  = .5, depth = .179,
#'   l = 12.297
#' )
#'
#' ## All examples at once:
#' data(b31gdata)
#' with(b31gdata, b31crvl(maop, d, wth, smys, def, depth, l))
#'
b31crvl <- function(maop, d, wth, smys, def = .72, depth, l) {
  checkmate::assert_double(
    maop,
    lower = 25.4, upper = 1.27e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    d,
    lower = 3.93e-2, upper = 1.27e5, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    wth,
    lower = 1.15e-2, upper = 1.275e4, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    smys,
    lower = 1e3, upper = 3e5, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    def, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    depth,
    lower = 0, upper = 2.54e4, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    l,
    lower = 0, upper = 1.275e4, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(maop), length(d), length(wth), length(smys), length(def),
    length(depth), length(l)
  )))
  checkmate::assert_true(all(d - 2 * wth > 0.02))  # in inch

  pipe <- data.frame(
    maop = maop, d = d, wth = wth, smys = smys, def = def, depth = depth, l = l,
    design_pressure = trunc(b31gdep(d, wth, smys, def)),
    status = b31gops(wth, depth), A = b31gafr(d, wth, l)
  )

  pipe[["safe_pressure"]] <- b31gsap(
    pipe[["design_pressure"]], d, wth, depth, l
  )
  pipe[["allowed_corrosion_length"]] <- b31gacl(
    pipe[["design_pressure"]], maop, d, wth, depth, l
  )
  pipe[["allowed_corrosion_depth"]] <- b31gacd(
    pipe[["design_pressure"]], maop, d, wth, l
  )
  pipe[["pressure_exceeding"]] <- maop > pipe[["safe_pressure"]]
  pipe[["AP"]] <- ifelse(
    is.infinite(pipe[["allowed_corrosion_length"]]),
    5.0,
    round(pipe[["allowed_corrosion_length"]] / sqrt(d * wth) / 1.12, 3)
  )
  class(pipe) <- c("crvl", class(pipe))
  pipe
}

#' @export
print.crvl <- function(x, ...) {
  if (nrow(x) > 1) {
    NextMethod("print")
    return(invisible(NULL))
  }

  cat(c(
    c(
      paste(
        "Corrosion depth is less than 10 % of pipe wall.",
        "No resrictions on operation"
      ),
      "",
      "Alert! Corrosion depth exceeds 80 % of pipe wall! Pipe must be replaced!"
    )[x[["status"]]],
    "-- Calculated data --",
    sprintf("Intermediate factor (A) = %02.3f", x[["A"]]),
    sprintf("Design pressure = %02i PSI; Safe pressure = %02i PSI",
            x[["design_pressure"]], x[["safe_pressure"]]),
    c(
      paste(
        "Repair or replace pipe because corrosion depth exceeds 80 %",
        "of pipe wall!"
      ),
      sprintf("Pipe may be operated safely at MAOP, %02i PSI", x[["maop"]]),
      "MAOP exceeds design pressure P. Verify that this variance is valid",
      sprintf(
        paste(
          "Reduce operating pressure so it will not exceed %02i PSI,",
          "and so operate legally and safely", collapse = ""
        ),
        x[["safe_pressure"]]
      )
    )[c(
      x[["depth"]] > .8 * x[["wth"]], x[["safe_pressure"]] >= x[["maop"]],
      x[["maop"]] > x[["design_pressure"]], x[["pressure_exceeding"]]
    )],
    sprintf(
      paste(
        "With corrosion length %02.3f inch, maximum allowed corrosion",
        "depth is %02.4f inch; A = %2.3f"
      ),
      x[["l"]], x[["allowed_corrosion_depth"]], x[["A"]]
    ),
    sprintf(
      paste(
        "With corrosion depth %02.3f inch, maximum allowed corrosion",
        "length is %02.4f inch; A = %02.3f"
      ),
      x[["depth"]], x[["allowed_corrosion_length"]], x[["AP"]]
    ),
    sprintf(
      "But %02.3f inch exceeds allowable corrosion depth!!!", x[["depth"]]
    )[x[["depth"]] > .8 * x[["wth"]]]
  ), sep = "\n")
}
