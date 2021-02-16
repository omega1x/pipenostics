# pipenostics

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/pipenostics)](https://cran.r-project.org/package=pipenostics)
[![CodeFactor](https://www.codefactor.io/repository/github/omega1x/pipenostics/badge)](https://www.codefactor.io/repository/github/omega1x/pipenostics)
![R-CMD-check](https://github.com/omega1x/pipenostics/workflows/R-CMD-check/badge.svg)
[![Build
Status](https://travis-ci.com/omega1x/pipenostics.svg?branch=master)](https://travis-ci.com/omega1x/pipenostics)

[R-package](https://cran.r-project.org/package=pipenostics) for
diagnostics, reliability and predictive maintenance of pipeline systems.

------------------------------------------------------------------------

## Table of contents
-   [Preface](#preface)
-   [Installation](#installation)
-   [Description](#description)
-   [Details](#details)
    -   [Corrosion diagnostics](#corrosion-diagnostics)
    -   [Heat losses](#heat-losses)
    -   [Tracing of thermal-hydraulic regime](#tracing-of-thermal-hydraulic-regime)


## Preface

Aiming for digital transformation of technical engineering departments
of heat generating and heat transferring companies the package
aggregates to some extent the separate knowledge about engineering,
reliability, diagnostics and predictive maintenance of pipeline systems.
For the present time the package contains utilities for processing
corrosion data commonly gathered by *inline inspection*, as well as
emperical models for calculations of local thermal-hydraulic regimes of
district heating network.


## Installation

For the stable release (if any), just install the latest version from
[CRAN](https://cran.r-project.org/package=pipenostics):

    install.packages("pipenostics")

For the development version, use
[devtools](https://cran.r-project.org/package=devtools):

    devtools::install_github("omega1x/pipenostics")

## Description

The motivation for the package was to aggregate to some extent the
separate knowledge about engineering, reliability, diagnostics and
predictive maintenance of pipeline systems suitable for pipe holders.
Distributing such knowledge with
[R-package](https://cran.r-project.org/package=pipenostics) seemed the
most attractive option for white-collar engineers, having utilized
spreadsheet software as a mainstream.

Aiming to avoid portability and accessibility problems made us search
ways to restrict source code development by functionality of few
external packages. It reasonably helps to use the package inside
[ML-services](https://docs.microsoft.com/en-us/sql/machine-learning/sql-server-machine-learning-services)
traditionally leveraged in large companies maintaining district heating
systems.

## Details

Since most functions have native argument vectorization usage of those
functions with fast
[data.table](https://cran.r-project.org/package=data.table) framework is
strongly encourage when processing large data sets. For that purpose
arguments for all package functions are thoroughly checked for type
consistency and physical sense using asserts and tests from
[checkmate](https://cran.r-project.org/package=checkmate) package.
Moreover, in package documentation we borrow type designations according
to [checkmate](https://cran.r-project.org/package=checkmate) notation.

### Corrosion diagnostics

The next values describing technological conditions, material properties
of pipe and defect parameters are used as arguments throughout the most
functions concerning corrosion diagnostics:

-   *maop* - maximum allowable operating pressure -
    [MAOP](https://en.wikipedia.org/wiki/Maximum_allowable_operating_pressure)
    in [PSI](https://en.wikipedia.org/wiki/Pounds_per_square_inch)
-   *d* - nominal outside diameter of the pipe,
    [inch](https://en.wikipedia.org/wiki/Inch)
-   *wth* - nominal wall thickness of the pipe,
    [inch](https://en.wikipedia.org/wiki/Inch)
-   *smys* - specified minimum yield of stress -
    [SMYS](https://en.wikipedia.org/wiki/Specified_minimum_yield_strength)
    as a characteristics of steel strength,
    [PSI](https://en.wikipedia.org/wiki/Pounds_per_square_inch)
-   *depth* - measured maximum depth of the corroded area,
    [inch](https://en.wikipedia.org/wiki/Inch)
-   *l* - measured maximum longitudial length of the corroded area,
    [inch](https://en.wikipedia.org/wiki/Inch)

#### ASME B31G-1991

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
of the pipeline system?

Thus, one of the needs of the pipeline industry has been a procedure
that will help operators, particularly field personnel, make decisions
on existing pipelines, when exposed for any purpose, as to whether any
corroded region may be left in service or whether it needs to be
repaired or replaced. Such determinations must be based upon sound
research and extensive testing in order to provide safe and conservative
guidelines on which to base field decisions.

[ASME
B31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf)
provides procedures to assist in this determination.

*Appendix A* to [ASME
B31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf)
shows the source code for determining the allowable length and maximum
allowable working pressure. The *b31g-* and `b31crvl()` functions
reproduce the idea of *CRVL.BAS*. They are natively vectorized.

Usage of `b31crvl()` function that imitates the output of *CRVL.BAS* is
presented in the next example:

    b31crvl(maop = 910, d = 30, wth = .438, smys = 52000, def  = .72, depth = .1, l = 7.5)

    -- Calculated data --
    Intermediate factor (A) = 1.847
    Design pressure = 1093 PSI; Safe pressure = 1093 PSI
    Pipe may be operated safely at MAOP, 910 PSI
    With corrosion length 7.500 inch, maximum allowed corrosion depth is 0.2490 inch; A = 1.847
    With corrosion depth 0.100 inch, maximum allowed corrosion length is Inf inch; A = 5.000

#### ASME B31G-2012

An effort was undertaken to update [ASME
B31G-1991](https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf)
up to [ASME
B31G-2012](https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines)
document to recognize certain other corrosion evaluation methods that
have proven sound and that have seen successful use in the pipeline
industry. Incorporation of these other methods provides us with a
formalized framework within which to use such methodologies.

Nevertheless, to preserve simplicity of traditional inline measurements
during inspections we only consider **Analysis Level 1**. As noted in
[ASME
B31G-2012](https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines)
**Level 1** evaluation is quite suitable for use in prioritizing
corrosion defects identified by inline inspection.

#### Other models

Other approaches for operating with corrosion data are mostly aimed on
failure pressure calculations. Models like `dnvpf()`, `shell92pf()`, and
`pcorrcpf()` assume different shapes of corrosion defects and usage
conditions for some cases. So, it is encouraged first to find out which
model is most suitable for solving some real world problem.

### Heat losses

Heat loss is the energy characteristic of district heating networks. It
is the amount of heat energy spent on the transportation and
distribution of heat energy from the source to the consumers.

Heat losses depend on the operating temperature, technical condition,
volume and configuration of the district heating network, as well as on
climatic factors. Heat losses are additive being the sum of the heat
losses of individual pipeline segments.

Determination of heat losses for pipeline segments hereinafter is called
*heat loss localization*.

It is assumed that actual heat loss (*Q*<sub>*A**H**L*</sub>) of
pipeline segment has two contributions: normative heat loss
(*Q*<sub>*N**H**L*</sub>) and extra-normative heat loss
(*Q*<sub>*E**x**N**H**L*</sub>). So we can write:

*Q*<sub>*A**H**L*</sub> = *Q*<sub>*N**H**L*</sub> + *Q*<sub>*E**x**N**H**L*</sub>, *Q*<sub>*E**x**N**H**L*</sub> &gt; 0

Localization of *Q*<sub>*E**x**N**H**L*</sub> is an important part of
health maintenance activities of district heating network operation. One
can determine *Q*<sub>*E**x**N**H**L*</sub> of pipeline segment as a
positive difference between *Q*<sub>*A**H**L*</sub> and
*Q*<sub>*N**H**L*</sub> and it is the most natural way. For that purpose
[Minenergo-325](http://docs.cntd.ru/document/902148459) and
[Minenergo-278](http://www.complexdoc.ru/ntdtext/547103) methods for
postulating *Q*<sub>*N**H**L*</sub> are considered.

[Minenergo-325](http://docs.cntd.ru/document/902148459) lists legally
affirmed maximum values of heat flux that is allowed to be emitted by
steel pipes (see `m325nhl()`). Higher emission is treated as
*Q*<sub>*E**x**N**H**L*</sub>.
[Minenergo-278](http://www.complexdoc.ru/ntdtext/547103) gives method
for engineering calculation of *Q*<sub>*N**H**L*</sub> considering
technical condition of pipeline segment (see `m278hlcha()`,
`m278hlund()`, and `m278hlair()`).

### Tracing of thermal-hydraulic regime

Localization of extra-normative heat losses
*Q*<sub>*E**x**N**H**L*</sub> could be performed if they know
thermal-hydraulic regime of district heating network for each pipeline
segment. In most cases thermal-hydraulic field (values of temperature,
pressure and heat carrier consumption) is measured only on heat-supply
origins and near consumers, i.e. mostly on terminal nodes of the
pipeline network. Middle segments of the network are rarely equipped and
thus are not sensor-measured. For restoring of thermal-hydraulic field
at each pipeline segment they can trace temperature, pressure and
heat-carrier consumption using hydraulic and thermal laws and
engineering characteristics of each pipe. Since only normative values of
heat flux are accessible for calculations the restored thermal-hydraulic
field is a normative (not actual) one. Nevertheless, even tracing small
networks may produce local inconsistencies in restored normative field
and those facts signal for possible extra-normative heat-losses in-situ.
In accordance to sensor positions forward (see `m325tracefw()`,
`m325traceline()`) and backward (see `m325tracebw()`,
`m325traceline(forward = FALSE)`) tracing can be performed for the
linear and the bunched pipelines on the basis of
[Minenergo-325](http://docs.cntd.ru/document/902148459) norms.
