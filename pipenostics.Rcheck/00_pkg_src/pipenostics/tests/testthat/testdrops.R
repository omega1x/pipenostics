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