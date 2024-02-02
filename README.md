# pipenostics

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/omega1x/pipenostics/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/omega1x/pipenostics/actions/workflows/R-CMD-check.yml)
[![pages-build-deployment](https://github.com/omega1x/pipenostics/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/omega1x/pipenostics/actions/workflows/pages/pages-build-deployment)
[![pipenostics status badge](https://omega1x.r-universe.dev/badges/pipenostics)](https://omega1x.r-universe.dev)
[![lintr](https://github.com/omega1x/pipenostics/actions/workflows/lintr.yml/badge.svg)](https://github.com/omega1x/pipenostics/actions/workflows/lintr.yml)
[![CodeFactor](https://www.codefactor.io/repository/github/omega1x/pipenostics/badge)](https://www.codefactor.io/repository/github/omega1x/pipenostics)
[![codecov](https://codecov.io/gh/omega1x/pipenostics/branch/master/graph/badge.svg?token=LMVLTBPAY5)](https://codecov.io/gh/omega1x/pipenostics)

[R-package](https://cran.r-project.org/package=pipenostics) for
diagnostics, reliability and predictive maintenance of pipeline systems.

------------------------------------------------------------------------

## Table of contents

- [Intro](#intro)
- [Installation](#installation)
- [Usage examples](#usage-examples)
  - [Corrosion diagnostics](#corrosion-diagnostics)
  - [Probability of failure](#probability-of-failure)
  - [Regime tracing](#regime-tracing)
- [Developer notes](#developer-notes)
- [Underlying concepts](#underlying-concepts)
  - [Corrosion diagnostics](#corrosion-diagnostics)
  - [Probability of failure](#probability-of-failure)
  - [Heat loss](#heat-loss)
  - [Tracing of thermal-hydraulic regime](#tracing-of-thermal-hydraulic-regime)

## Intro

The package aggregates to some extent the separate knowledge concerning
engineering, reliability, diagnostics and predictive maintenance of pipeline
systems. For the present time the package contains utilities for processing
corrosion data commonly gathered by *inline inspection*, as well as
empirical models for calculations of local thermal-hydraulic regimes of
district heating network.

## Installation

For the latest version leverage [r-universe](https://omega1x.r-universe.dev/ui#builds):

```R
install.packages("pipenostics", repos = "https://omega1x.r-universe.dev")
```

> &#9888; Starting from version 0.1.8 the package is not supported on [CRAN](https://cran.r-project.org/) due to its resource limitations of checking parallel algorithms

## Usage examples

### Corrosion diagnostics examples

By using of `b31crvl()` simply imitate the output of *CRVL.BAS* which is the honored software for determining the allowable length and maximum
allowable working pressure presented in [ASME B31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf):

```R
library(pipenostics)
    
b31crvl(maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1, l = 7.5)
```

```txt
-- Calculated data --
Intermediate factor (A) = 1.847
Design pressure = 1093 PSI; Safe pressure = 1093 PSI
Pipe may be operated safely at MAOP, 910 PSI
With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847
With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000
```

### Probability of failure examples

Let's consider a pipe in district heating network with

```R
diameter           <- 762         # [mm]
wall_thickness     <-  10         # [mm]
UTS                <- 434.3697    # [MPa]
```

which transfers heat-carrier (water) at

```R
operating_pressure <-   0.588399  # [MPa]
temperature        <-  95         # [°C]
```

During *inline inspection* four corroded areas (defects) are detected with:

```R
depth  <- c(2.45,  7.86,   7.93,   8.15)  # [mm]
```

whereas the length of all defects is not greater 200 mm:

```R
length <- rep(200, 4)  # [mm]
print(length)
```

```R
[1] 200 200 200 200
```

Corrosion rates in radial and in longitudinal directions are not well-known and
may vary in range `.01` - `.30` mm/year:

```R
rar = function(n) stats::runif(n, .01, .30) / 365
ral = function(n) stats::runif(n, .01, .30) / 365
```

Then probabilities of failure (POFs) related to each corroded area are near:

```R
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv")
```

```txt
pipenostics::mepof: process case [4/4] - 100 % . All done, thanks!                 
```

```R
print(pof)
```

```R
[1] 0.000000 0.252935 0.368741 0.771299
```

So, the POF of the pipe is near

```R
print(max(pof))
```

```R
[1] 0.771299
```

The value of POF changes in time. So, in a year after *inline inspection* of
the pipe we can get something near

```R
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv", days = 365)
```

```txt
pipenostics::mepof: process case [4/4] - 100 % . All done, thanks!             
```

```R
print(pof)
```

```R
[1] 0.000000 0.526646 0.647422 0.928825
```

For entire pipe we get something near:

```R
print(max(pof))
```

```R
[1] 0.928825
```

Two years ago before *inline inspection* the pipe state was rather good:

```R
pof <- mepof(depth, length, rep(diameter, 4), rep(wall_thickness, 4),
             rep(UTS, 4), rep(operating_pressure, 4), rep(temperature, 4),
             rar, ral, method = "dnv", days = -2 * 365)
```

```txt
pipenostics::mepof: process case [4/4] - 100 % . All done, thanks!
```

```R
print(pof)
```

```R
[1] 0.000000 0.040849 0.072734 0.272358
```

For entire pipe we get something near:

```R
print(max(pof))
```

```R
[1] 0.272358
```

### Regime tracing

Let's consider the next 4-segment tracing path:

![m325regtrace](https://raw.githubusercontent.com/omega1x/pipenostics/a26e1171f05d3cd4f2c25a71ccde9947a095409f/.src/svg-graphics/m325regtrace.svg)

Suppose we have the next sensor readings for *forward tracing*:

```R
t_fw <- 130         # [°C]
p_fw <-   0.588399  # [MPa]
g_fw <- 250         # [ton/hour]
```

Let's discharges to network for each pipeline segment are somehow determined as

```R
discharges <- seq(0, 30, 10)  # [ton/hour]
print(discharges)
```

```R
[1]  0 10 20 30
```

Then the calculated regime (red squares) for forward tracing is

```R
regime_fw <- m325traceline(t_fw, p_fw, g_fw, discharges, forward = TRUE)
print(regime_fw)
```

```R
$temperature
[1] 129.1799 128.4269 127.9628 127.3367

$pressure
[1] 0.5878607 0.5874226 0.5872143 0.5870330

$flow_rate
[1] 250 240 220 190
```

> For further examples go to package function descriptions.

## Developer notes

Aiming to avoid portability and accessibility problems made us search
ways to restrict source code development by functionality of few
external packages. It reasonably helps to use the package inside
[ML-services](https://docs.microsoft.com/en-us/sql/machine-learning/sql-server-machine-learning-services)
traditionally leveraged in large companies maintaining district heating
systems.

Since most functions have native argument vectorization usage of those
functions with fast
[data.table](https://cran.r-project.org/package=data.table) framework is
strongly encouraged when processing large data sets. For that purpose
arguments for all package functions are thoroughly checked for type
consistency and physical sense using asserts and tests from
[checkmate](https://cran.r-project.org/package=checkmate) package.
Moreover, in package documentation we borrow type designations according
to [checkmate](https://cran.r-project.org/package=checkmate) notation.

## Underlying concepts

### Corrosion diagnostics

It is recognized by pipeline companies that some sections of high
pressure pipelines particularly those installed a number of years ago,
have experienced some corrosion. Where corrosion is found, pipeline
operators have been deeply concerned about the need for a method of
determining the remaining strength of these corroded areas. If the
corrosion does not penetrate the pipe wall, what is the pressure
containing capability of the remaining pipe metal in terms of its
ability to continue to operate safely at the maximum allowable operating
pressure
([MAOP](https://en.wikipedia.org/wiki/Maximum_allowable_operating_pressure))
of pipeline system?

Thus, one of the needs of pipeline industry has been a procedure
that will help operators, particularly field personnel, make decisions
on existing pipelines, when exposed for any purpose, as to whether any
corroded region may be left in service or whether it needs to be
repaired or replaced. Such determinations must be based upon sound
research and extensive testing in order to provide safe and conservative
guidelines on which to base field decisions.

[ASMEB31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf)
and [ASME
B31G-2012](https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines)
codes have proven sound and have seen successful use in the pipeline
industry providing users with such required formalized framework for a very long period of time. That is why failure pressure calculators
`b31gpf()` and `b31gmodpf()` are widely used in assessment of POFs.

To preserve simplicity of traditional inline measurements
during inspections we hereinafter consider only **Analysis Level 1** in this *R*-package, since as noted in
[ASME
B31G-2012](https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines)
**Level 1** evaluation is quite suitable for use in prioritizing
corrosion defects identified by *inline inspection*.

Other approaches for operating with corrosion data presented in the package are aimed on
failure pressure calculations. Models like `dnvpf()`, `shell92pf()`, and
`pcorrcpf()` assume different shapes of corrosion defects and usage
conditions for some cases. So, it is encouraged first to find out which
model is most suitable for solving some real world problem.

For the sake of simplicity and transparency the next values describing technological conditions, material properties
of pipe and defect parameters are used as arguments throughout the most
functions concerning corrosion diagnostics:

- *maop* - maximum allowable operating pressure -
  [MAOP](https://en.wikipedia.org/wiki/Maximum_allowable_operating_pressure)
  in [PSI](https://en.wikipedia.org/wiki/Pounds_per_square_inch)
- *d* - nominal outside diameter of pipe,
    [inch](https://en.wikipedia.org/wiki/Inch), or [mm](https://en.wikipedia.org/wiki/Millimetre)
- *wth* - nominal wall thickness of pipe,
    [inch](https://en.wikipedia.org/wiki/Inch), or [mm](https://en.wikipedia.org/wiki/Millimetre)
- *smys* - specified minimum yield of stress -
    [SMYS](https://en.wikipedia.org/wiki/Specified_minimum_yield_strength)
    as a characteristics of steel strength, [PSI](https://en.wikipedia.org/wiki/Pounds_per_square_inch)
- *uts* - ultimate tensile strength -
  [UTS](https://en.wikipedia.org/wiki/Ultimate_tensile_strength) or
  specified minimum tensile strength (SMTS) as another characteristic of steel strength, [MPa](https://en.wikipedia.org/wiki/Pascal_(unit))
- *depth* - measured maximum depth of the corroded area,
    [inch](https://en.wikipedia.org/wiki/Inch), or [mm](https://en.wikipedia.org/wiki/Millimetre)
- *l* - measured maximum longitudinal length of the corroded area,
    [inch](https://en.wikipedia.org/wiki/Inch), or [mm](https://en.wikipedia.org/wiki/Millimetre)

In the course of further development of functionality of this package,
some revisions or supplements to the existing concept are not excepted.

### Probability of failure

Consistent estimate of failure for pipeline systems plays a critical role in optimizing their operation.
To prevent pipeline failures due to growing corrosion defects it is necessary to assess the pipeline failure probability (POF) during a certain period, taking into account its actual level of defectiveness.

The pipeline failure is preceded by limit state which comes when the burst pressure, considered as a random variable, reaches an unacceptable level, or when the defect depth, also a random variable, exceeds the
predetermined limit value.

Up to now no methods existed which would give absolutely correct POF assessments. Nevertheless the stochastic nature of corrosion processes clearly suggests exploiting of [Monte-Carlo simulations](https://en.wikipedia.org/wiki/Monte_Carlo_method#Monte_Carlo_and_random_numbers) (MC).
Meanwhile the lack of comprehensive knowledge of stochastic properties of characteristics of pipe and of its defects aids in embracing of  [Principle of maximum entropy](https://en.wikipedia.org/wiki/Principle_of_maximum_entropy)
which allows to avoid doubtful and excessive preferences and specifications when choosing probability distribution models for failure factors and for *inline inspection* measurements.

Package function `mepof()` is designed to calculate probability of failure (POF) of the corroded pipe by
[MC](https://en.wikipedia.org/wiki/Monte_Carlo_method#Monte_Carlo_and_random_numbers), assigning maximum entropy
for stochastic nature of corroded area length and depth, as well as engineering characteristics of pipe with
thermal-hydraulic regime parameters.

### Heat loss

*Heat loss* is the energy characteristic of district heating networks. It
is the amount of heat energy spent on the transportation and
distribution of heat energy from the source to the consumers.

*Heat loss* depends on the operating temperature, technical condition,
volume and configuration of district heating network, as well as on
climatic factors. *Heat loss* is additive being the sum of *heat
losses* of individual pipeline segments.

Determination of heat loss for pipeline segments hereinafter is called
*heat loss localization*.

It is assumed that actual heat loss ($Q_{\text{AHL}}$, [[kcal](https://en.wikipedia.org/wiki/Calorie)]) of
pipeline segment in certain period of time (duration) has two contributions: normative heat loss
($Q_{\text{NHL}}$, [[kcal](https://en.wikipedia.org/wiki/Calorie)])) and extra-normative heat loss
($Q_{\text{ExNHL}}$, [[kcal](https://en.wikipedia.org/wiki/Calorie)])). So we can write:

$$ Q_{\text{AHL}} = Q_{\text{NHL}} + Q_{\text{ExNHL}},\  Q_{\text{ExNHL}}> 0 $$

Localization of $Q_{\text{ExNHL}}$ is an important part of
health maintenance activities of district heating network operation. One
can determine $Q_{\text{ExNHL}}$ of pipeline segment as a
positive difference between $Q_{\text{AHL}}$ and
$Q_{\text{NHL}}$ and it is the most natural way. For that purpose
[Minenergo-325](https://docs.cntd.ru/document/902148459) and
[Minenergo-278](https://docs.cntd.ru/document/1200035568) methods for
postulating $Q_{\text{NHL}}$ are considered.

[Minenergo-325](https://docs.cntd.ru/document/902148459) lists legally
affirmed maximum values of *specific heat loss power* ($q_s$, *kcal/m/h*) which is allowed for fault-free
steel pipes (see `m325nhl()`), so that normative heat loss of *L*-meter length pipe for a duration $\tau$ is

$$Q_{NHL} = L\int_{\tau}^{}q_s(\tau)d\tau$$

> &#8505; *Specific heat loss power* may also be referred as specific [rate of heat flow](https://en.wikipedia.org/wiki/Rate_of_heat_flow).

> &#9888; Specific powers (specific rates of heat flow) higher than $q_s$ contribute to $Q_{\text{ExNHL}}$.

[Minenergo-278](https://docs.cntd.ru/document/1200035568) gives mathematical models
for calculation of *specific heat loss power* ($q_s$) as a function of
thermal-hydraulic regime and technical condition of pipeline segment (see `m278hlcha()`,
`m278hlund()`, and `m278hlair()`).

*Specific heat loss power* ($q_{s}$, *kcal/m/h*) of cylindrical pipe can be more naturally expressed via the value of [heat flux](https://en.wikipedia.org/wiki/Heat_flux) ($\phi_q$, [W/m^2](https://en.wikipedia.org/wiki/Heat_flux)) output by pipe wall through unit area:

$$q_s = \frac{3600}{4186.8} \cdot \phi_q \cdot \pi d$$

where $d$ [m](https://en.wikipedia.org/wiki/Metre) is an outside diameter of pipe, and $\frac{3600}{4186.8}\pi = 2.701283$ (*kcal/h/W*) is a dimension factor (see `loss_flux()`, `flux_loss()`).

*Heat loss power for a pipe* ($q_p$, *kcal/hour*) may be calculated as 
$$q_p = q_s L$$

where $L$ is a pipe length.

### Tracing of thermal-hydraulic regime

Localization of extra-normative heat loss
$Q_{\text{ExNHL}}$ could be performed if they know
thermal-hydraulic regime of district heating network for each pipeline
segment. In most cases thermal-hydraulic field (values of temperature,
pressure and heat carrier flow rate) is measured only on heat-supply
origins and near consumers, i.e. mostly on terminal nodes of
pipeline network. Middle segments of network are rarely equipped and
thus are not sensor-measured. For restoring of thermal-hydraulic field
at each pipeline segment they can trace temperature, pressure and
heat-carrier flow rate using hydraulic and thermal laws and
engineering characteristics of each pipe. Since only normative values of
heat flux are accessible for calculations the restored thermal-hydraulic
field is a normative (not actual) one. Nevertheless, even tracing small
networks may produce local inconsistencies in restored normative field
and those facts signal for possible extra-normative heat loss in-situ.
In accordance to sensor positions forward (see `m325tracefw()`,
`m325traceline()`) and backward (see `m325tracebw()`,
`m325traceline(forward = FALSE)`) tracing can be performed for the
linear and the bunched pipelines (also with massive data lack) on the basis of
[Minenergo-325](https://docs.cntd.ru/document/902148459) norms.
