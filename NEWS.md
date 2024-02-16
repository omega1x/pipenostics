# pipenostics 0.2.0

## Backlog

- Implement Lu Xing's two-harmonic model of ground temperature (Oklahoma State University).

In Equation 5-1, if $\beta = 1$, the simplified design model is the same as the two harmonic model
described in Chapter 4 and it can be used for the estimations of the typical year ground
temperatures. To estimate the peak ground temperatures of multiple years, $\beta = 1.6$ is chosen.

A constant soil diffusivity is now assumed at each site with a typical
value of $4.9 \times 10^{-7} \frac{m^2}{s}$. According to [Hendrickx et al. (2003)](https://www.researchgate.net/publication/220006673_Worldwide_distribution_of_soil_dielectric_and_thermal_properties), this value equals to
the one given for 60% saturated silt and clay soil.

## Current version

- function `geodist()` for calculating geographical objects is added.
- functions `traceline()`, `tracefw()`, and `tracebw()` were added to process district heating networks with user-provided heat loss.
- job log of `m325tracefw()` now does not contain duplicated traced flow paths.
- job logs of `m325tracefw()` and `m325tracebw()` now contain `loss`, `flux`, `Q`.
- function `m325tracebw()`now can trace partially sensor-equipped district networks.
- functions `m325dropt()` and `m325tracebwm()` were excluded.
- functions `flux_loss()` and `loss_flux()` were added to convert between *specific heat loss power*, [*kcal/m/h*] , and *heat flux*, [*W/m^2*].
- the heat loss related terminology was clarified and adjusted: terms *specific heat loss power*, [*kcal/m/h*],
  and *heat flux*, [*W/m^2*], are viewed separately while stay closely related. Term `consumption` replaced with `flow_rate`.
