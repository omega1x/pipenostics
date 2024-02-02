# pipenostics 0.2.0

## Tracing of thermal-hydraulic regime

### Current version
- The heat loss related terminology was clarified and adjusted: terms *specific heat loss power*, [*kcal/h/m*], and *heat flux*, [*W/m^2*], are viewed separately while stay closely related.
- functions `flux_loss()` and `loss_flux()` were added to convert between *specific heat loss power*, [*kcal/h/m*] , and *heat flux*, [*W/m^2*].
- functions `m325dropt()` and `m325tracebwm()` were excluded.
- function `m325tracebw()`now can trace partially sensor-equipped district networks.

### Further plans

- Unify job logs of `m325traceline()` and `m325tracefw()` with job log `m325tracebw()` (add `loss`, `flux`, `Q` and to job logs).
- Create `traceline()`, `tracefw()`, and `tracebw()` for user-provided loss.
- Enrich `m278soildata` dataset with data from Tymen monography. Possibly and rename the dataset to `soildata`)
- Correctly calculate outside diameter as argument to `flux_loss()` in `m325trace()` which now deals with inside diameter.
