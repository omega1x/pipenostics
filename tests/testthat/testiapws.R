library(pipenostics)

test_that("water pressure on saturation curve vioalates IAPWS-IF97", {
  expect_equal(
    if97pt4(),
    c(.353658941e-2, .263889776e1, .123443146e2),
    tolerance = 1e-8
  )
})

test_that("specific isobaric heat capacity of water vioalates IAPWS-IF97 in Region 1", {
  expect_equal(
    if97cptp1(),
    c(.417301218, .401008987, 0.465580682)*10,
    tolerance = 1e-8
  )
})


test_that("specific volume of water vioalates IAPWS-IF97 in Region 1", {
  expect_equal(
    if97vtp1(),
    c(.100215168e-2, .971180894e-3, .120241800e-2),
    tolerance = 1e-8
  )
})


test_that("dynamic viscosity of water vioalates IAPWS R12-08", {

  ## Release on the IAPWS Formulation 2008 for the
  ## Viscosity of Ordinary Water Substance (IAPWS R12-08)
  ## [http://www.iapws.org/relguide/visc.pdf]
  ##
  ## Table 4

  r12t4 <- read.csv(text =
#      [ºC]   [kg/m^3]  [Pa*s]*1e-6
"
temperature,   density,     dynvisc
     298.15,     998.0,  889.735100
     298.15,    1200.0, 1437.649467
     373.15,    1000.0,  307.883622
     433.15,       1.0,   14.538324
     433.15,    1000.0,  217.685358
     873.15,       1.0,   32.619287
     873.15,     100.0,   35.802262
     873.15,     600.0,   77.430195
    1173.15,       1.0,   44.217245
    1173.15,     100.0,   47.640433
    1173.15,     400.0,   64.154608
"
)

  with(r12t4, {
    expect_equal(
      r12dv(temperature, density),
      dynvisc,
      tolerance = 1e-8
    )
  })

  rm(r12t4)
})
