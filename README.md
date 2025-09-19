# pipenostics

[![License: GPLv3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN](https://www.r-pkg.org/badges/version/pipenostics)](https://cran.r-project.org/package=pipenostics)
[![pipenostics status badge](https://omega1x.r-universe.dev/badges/pipenostics)](https://omega1x.r-universe.dev)
[![R-CMD-check](https://github.com/omega1x/pipenostics/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/omega1x/pipenostics/actions/workflows/R-CMD-check.yml)
[![pages-build-deployment](https://github.com/omega1x/pipenostics/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/omega1x/pipenostics/actions/workflows/pages/pages-build-deployment)
[![CodeFactor](https://www.codefactor.io/repository/github/omega1x/pipenostics/badge)](https://www.codefactor.io/repository/github/omega1x/pipenostics)
[![codecov](https://codecov.io/gh/omega1x/pipenostics/branch/master/graph/badge.svg?token=LMVLTBPAY5)](https://app.codecov.io/gh/omega1x/pipenostics)

[R-package](https://cran.r-project.org/package=pipenostics) for
diagnostics, reliability and predictive maintenance of pipeline systems.

------------------------------------------------------------------------

## Intro

The package aggregates some of the individual knowledge regarding engineering,
reliability, diagnostics, and predictive maintenance of pipeline systems.
At the moment, the package includes utilities for processing corrosion data
commonly collected by *inline inspection*, as well as empirical models for
calculating local thermal-hydraulic conditions in district heating networks.
Additionally, the package provides a variety of useful tools and datasets that
can assist with solving a range of related issues.

## Installation

For the latest version leverage
[r-universe](https://omega1x.r-universe.dev/pipenostics):

```R
install.packages("pipenostics", repos = "https://omega1x.r-universe.dev")
```

> &#9888; Starting with *pipenostics 0.3.0*,
  [CRAN]( https://CRAN.R-project.org/package=pipenostics) releases have been
  discontinued due to the lack of full UTF-8 support on the
  *CRAN*-side and ambiguous URL restrictions in package documentation.

## Usage

For underlying concepts and other usage examples start with
[Concepts and Useful Notes](https://omega1x.github.io/pipenostics/articles/pipenostics.html).
