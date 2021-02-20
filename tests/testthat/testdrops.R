library(pipenostics)

test_that("*dropg* errs in consumption drop", {
  d <- as.double(c(800, 900, 1000, 1400, 1200))
  consumption <- .125*d
  adj <- c(450, -400, 950, -255, 1152)
  expect_equal(
    consumption - dropg(adj, d, consumption),
    c(75.96439, 134.72222, 65.70302,180.80580,  78.05995),
    tolerance = 1e-5
  )

  adjp <- list(
    c(100, 175, 175, -65, 125, -60),  # diameters of 4 discharge pipes and 2 recharge pipes, [mm]
    c(-300, -100, -65, 125, -60),  # diameter of 1 discharge pipe and 4 recharge pipes, [mm]
    c(950),  # diameter of 1 discharge pipe, [mm]
    c(-255), # diameter of 1 recharge pipe, [mm]
    c(50, 70, 1000, 32)  # diameter of 4 discharge pipes, [mm]
  )
  d <- c(800, 900, 1000, 1400, 1200)
  consumption <- .125*d

  expect_equal(
    consumption - dropg(adjp, d, consumption),
    c(75.96439, 134.72222, 65.70302, 180.80580, 78.05995),
    tolerance = 1e-5
  )
})


test_that("*dropp* errs in pressure drop", {
  expect_equal(
    dropp(len = c(200, 300)),
    c(0.0007000666, 0.0010500999)
  )
})


test_that("*dropt* errs in temperature drop", {
  pipeline <- list(
    year = 1968,
    laying = "channel",
    d = 700,
    l = 1000
  )
  operation_temperature <- c(130, 150)  # [°C]
  foo <- dropt(
      temperature = operation_temperature,
      flux = do.call(
        m325nhl,
        c(pipeline, temperature = list(operation_temperature))
      )
  )
  expect_equal(
    foo,
    c(1.366806, 1.433840),
    tolerance = 1e-6
  )

  expect_equal(
    foo,
    m325dropt(
      temperature = operation_temperature, year = 1968, laying = "channel",
      d = 700, len = 1000
    ),
    tolerance = 1e-6
  )

})