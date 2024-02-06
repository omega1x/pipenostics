# pipenostics 0.2.0

## Tracing of thermal-hydraulic regime

### Current version

- the heat loss related terminology was clarified and adjusted: terms *specific heat loss power*, [*kcal/h/m*],
  and *heat flux*, [*W/m^2*], are viewed separately while stay closely related. Term `consumption` replaced with `flow_rate`.
- functions `flux_loss()` and `loss_flux()` were added to convert between *specific heat loss power*, [*kcal/h/m*] , and *heat flux*, [*W/m^2*].
- functions `m325dropt()` and `m325tracebwm()` were excluded.
- function `m325tracebw()`now can trace partially sensor-equipped district networks.
- job logs of `m325tracefw()` and `m325tracebw()` now contain `loss`, `flux`, `Q`.

### Further plans

- Create `traceline()`, `tracefw()`, and `tracebw()` for user-provided loss.
- Implement Lu Xing's two-harmonic model of ground temperature (Oklahoma State University)
