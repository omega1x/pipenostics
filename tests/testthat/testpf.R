context("pf-calculations")
library(pipenostics)

test_that("dnvpf gives wrong results", {
  d     <- c(812.8, 219.0)  # [mm]
  wth   <- c( 19.1,  14.5)  # [mm]
  uts   <- c(530.9, 455.1)  # [N/mm^2]
  l     <- c(203.2, 200.0)  # [mm]
  depth <- c( 13.4,   9.0)  # [mm]

  expect_equal(
    dnvpf(d, wth, uts, depth, l), c(15.8663, 34.0118),
    tolerance = 1e-4
  )
})
