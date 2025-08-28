# pipenostics 0.2.1

## Backlog

- correct unit tests that are ignore for now. See the list of ignored tests in [.Rbuildignore](./.Rbuildignore) 
- think if they should change internal diameter to outside diameter in simple `trace*()` functions
- include `b36dwthv` into some tracers as an optional check
- introduce `wth` argument to *m325*-tracers, and correct `flux_loss` calculations
- in *m325*-tracers do not calculate total heat loss for a DAY since *a*-factor loss is introduced (is it right?)
- unify column names in `m325nhldata` and `b36pipedata`
- deprecate function `wth_d`
- add additional check for positivity of internal diameter $d - 2*wt$ (where $d$ denotes nominal (outside) diameter)
- spread diameter `d` and wall thickness `wth` range checkmates to all functions with those arguments
- add `rulc` - remain useful life calculator

## Current version
- dataset `m325nxdata` now contains values of *a*-factor, pipe widths, and pipe diameters in *mm*, so that it's valid *b36pipedata* data.
- function `b36dwthv` is introduced to perform validation of `d`/`wth` value pairs
- functions `m325nvl()`, `m325nml()` are introduced to calculate normative material loss of heat carrierin pipe
- issues with links in documentation are solved
- datasets `api5l3t` and `m325testbench` are renamed to `api5l3tdata` and `m325nxdata` appropriately for the purpose of unification with dataset naming rules
- functions `b36mass()`, `b36wth()` `b36d()` are introduced to calculate mass and geometric characteristics of actually manufactured pipes
- nominal specifications of the manufactured pipes now are represented in `b36pipedata`-dataset

## Version 0.2.0

- where possible and justified, support for multi-threaded data processing, based on the capabilities of the parallel package, has been added.
- functions for determining the state of water and steam ([IAPWS](http://www.iapws.org/)) have been excluded from the package due to the decision to use the built-in functions from [iapws](https://CRAN.R-project.org/package=iapws)-package imported from [CRAN](https://cran.r-project.org/).
- set of functions `mgtdhid()`, `mgtdhidt()`, `mgtdhgeo()`, `mgtdhgeot()` are introduced to interface with *Modified Ground Temperature Double Harmonic Model*.
- function `meteos()` is introduced to get a list of weather stations
- functions `geodist()` and `geoarea()` for calculating geographical metrics are added.
- functions `traceline()`, `tracefw()`, and `tracebw()` were added to process district heating networks with user-provided heat loss.
- job log of `m325tracefw()` now does not contain duplicated traced flow paths.
- job logs of `m325tracefw()` and `m325tracebw()` now contain `loss`, `flux`, `Q`.
- function `m325tracebw()`now can trace partially sensor-equipped district networks.
- functions `m325dropt()` and `m325tracebwm()` were excluded.
- functions `flux_loss()` and `loss_flux()` were added to convert between *specific heat loss power*, [*kcal/m/h*] , and *heat flux*, [*W/m^2*].
- the heat loss related terminology was clarified and adjusted: terms *specific heat loss power*, [*kcal/m/h*],
  and *heat flux*, [*W/m^2*], are viewed separately while stay closely related. Term `consumption` replaced with `flow_rate`.
