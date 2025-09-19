# pipenostics v0.3.0

- Active [issues](https://github.com/omega1x/pipenostics/issues/) has been
  solved.
- Function `wth_d` has been deprecated.
- Breaking changes have been made to *m325*-tracers and *flux-loss*
  recalculations.
- In appropriate functions the checkmate asserts specify `~0.29 mm` as a minimum
  allowable value for pipe thickness `wth`.
- In `m325nhldata` column *diameter* has been renamed to *d* that unifies it
  with `b36pipedata` column names.
- Additional check for positivity of internal diameter now is made inside those
  functions which accept outside diameter and wall thickness as arguments.
- Dataset `m325nxdata` now contains values of *a*-factor, pipe widths, and pipe
  diameters in *mm*, so that it's valid `b36pipedata` data.
- Function `b36dwthv` is introduced to perform validation of `d`/`wth` value
  pairs.
- Functions `m325nvl()`, `m325nml()` are introduced to calculate normative
  material loss of heat carrierin pipe.
- Issues with links in documentation are solved.
- Datasets `api5l3t` and `m325testbench` are renamed to `api5l3tdata` and
  `m325nxdata` appropriately for the purpose of unification with dataset naming
  rules.
- Functions `b36mass()`, `b36wth()` `b36d()` are introduced to calculate mass
  and geometric characteristics of actually manufactured pipes nominal
  specifications of the manufactured pipes now are represented in
  `b36pipedata`-dataset.

# pipenostics v0.2.0

- Where possible and justified, support for multi-threaded data processing,
  based on the capabilities of the parallel package, has been added.
- Functions for determining the state of water and steam
  ([IAPWS](https://iapws.org/)) have been excluded from the package due to
  the decision to use the built-in functions from
  [iapws](https://CRAN.R-project.org/package=iapws)-package imported from
  [CRAN](https://cran.r-project.org/).
- Set of functions `mgtdhid()`, `mgtdhidt()`, `mgtdhgeo()`, `mgtdhgeot()`
  are introduced to interface with
  *Modified Ground Temperature Double Harmonic Model*.
- Function `meteos()` is introduced to get a list of weather stations
- Functions `geodist()` and `geoarea()` for calculating geographical metrics are
  added.
- Functions `traceline()`, `tracefw()`, and `tracebw()` were added to process
  district heating networks with user-provided heat loss.
- Job log of `m325tracefw()` now does not contain duplicated traced flow paths
- job logs of `m325tracefw()` and `m325tracebw()` now contain `loss`, `flux`,
  and `Q`.
- Function `m325tracebw()`now can trace partially sensor-equipped district
  networks.
- Functions `m325dropt()` and `m325tracebwm()` were excluded.
- Functions `flux_loss()` and `loss_flux()` were added to convert between
  *specific heat loss power*, [*kcal/m/h*] , and *heat flux*, [*W/m²*].
- The heat loss related terminology was clarified and adjusted:
  terms *specific heat loss power*, [*kcal/m/h*], and *heat flux*, [*W/m²*],
  are viewed separately while stay closely related. Term `consumption` replaced
  with `flow_rate`.
