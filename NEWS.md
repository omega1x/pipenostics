# pipenostics 0.2.0

## Backlog

- transit to *iapws* package
- improve code with functionality of package `parallel`

## Current version

- set of functions `mgtdhid()`, `mgtdhidt`, `mgtdhgeo`, `mgtdhgeot` are introduced to interface with *Modified Ground Temperature Double Harmonic Model*.
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
