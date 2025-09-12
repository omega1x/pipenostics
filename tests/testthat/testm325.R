library(pipenostics)

test_that("*m325nhl* errs in normative heat loss", {
  expect_equal(
    m325nhl(
      1980, "underground", d = 73, temperature = 65, beta = c(FALSE, TRUE)
    ),
    c(65.500, 75.325),
    tolerance = 1e-3
  )
  expect_equal(
    m325nhl(2000, "channel", d = 73, temperature = 65, beta = c(FALSE, TRUE)),
    c(17.533, 21.040),
    tolerance = 1e-3
  )

  with(m325nhldata, {
    expect_equal(
      m325nhl(epoch, laying, exp5k, insulation, as.double(d), temperature),
      loss
    )
  })
})

test_that("*m325beta* errs in calculation", {
  data(m325nhldata)
  expect_equal(
    unique(m325beta(m325nhldata[["laying"]], as.double(m325nhldata[["d"]]))),
    c(1.15, 1.2)
  )
})

a   <- c(0.0025, 0.0010)     # [h⁻¹]
p   <- c(0.588399, 0.588399) # [MPa]
tmp <- c(60, 130)            # [°C]

test_that("*m325nvl* erros in calculations", {
  expect_equal(
    m325nvl(c(0, 0.0025)), c(0.0000000000, 0.0009511486)
  )
  expect_equal(
    m325nvl(a) * iapws::if97("rho", p, k_c(tmp))[, 1],
    c(0.9353813, 0.3557285), # [kg/h]
    tolerance = 1e-6
  )
})

test_that("*m325nml* erros in calculations", {
  expect_equal(
    m325nml(tmp, p, a), c(0.9353813, 0.3557285) * 1e-3,
    tolerance = 1e-6
  )
})

rm(tmp, p, a)
