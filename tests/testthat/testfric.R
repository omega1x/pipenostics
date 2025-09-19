library(pipenostics)

test_that("estimation of Darcy friction factor in non-strict mode errs", {

  expect_equal(
    fric_romeo(c(2118517, 2000, 2118517), c(1e-6, 70e-3, 7e-3)),
    fric_buzzelli(c(2118517, 2000, 2118517), c(1e-6, 70e-3, 7e-3)),
    tolerance = 1e-3
  )

  expect_equal(
    fric_romeo(c(2118517, 2000, 2118517), c(1e-6, 70e-3, 7e-3)),
    fric_vatankhan(c(2118517, 2000, 2118517), c(1e-6, 70e-3, 7e-3)),
    tolerance = 1e-3
  )

  expect_equal(
    all(
      fric_romeo(c(2118517, 2000, 2118517), c(1e-6, 70e-3, 7e-3)) < 0.2
      &
      fric_romeo(c(2118517, 2000, 2118517), c(1e-6, 70e-3, 7e-3)) > 0.0
    ),
    TRUE
  )
})

test_that("estimation of Darcy friction factor in strict mode errs", {
  expect_equal(
    fric_romeo(c(2118517, 5500, 2118517), c(1e-6, 50e-3, 7e-3), TRUE),
    fric_buzzelli(c(2118517, 5500, 2118517), c(1e-6, 50e-3, 7e-3), TRUE),
    tolerance = 1e-3
  )

  expect_equal(
    fric_romeo(c(2118517, 5500, 2118517), c(1e-6, 50e-3, 7e-3), TRUE),
    fric_vatankhan(c(2118517, 5500, 2118517), c(1e-6, 50e-3, 7e-3), TRUE),
    tolerance = 1e-3
  )
  expect_equal(
    all(
      fric_romeo(
        c(2118517, 5500, 2118517), c(1e-6, 50e-3, 7e-3), TRUE
      ) < 0.2
      &
      fric_romeo(
        c(2118517, 5500, 2118517), c(1e-6, 50e-3, 7e-3), TRUE
      ) > 0.0
    ),
    TRUE
  )
})
