library(pipenostics)

test_that("*geodist* errs in calculation", {
  expect_equal(
    geodist(
      c(77.1539, 77.1539, 77.1539),
      c(-139.398, 120.398, -120.398)
      ,
      c(-77.1804, 77.1804, 77.1804),
      c(-139.55, 129.55, 129.55)
      ,
      6372795
    )
    ,
    c(17166029, 225883, 2332669)
    ,
    tolerance = 1e-6
  )
})

test_that("*geoarea* errs in calculation", {
  expect_equal(
    geoarea(
      lat1 = c(
        s28434 = 56.65, Miami   =  25.789106,  Hawaii       =   19.820680
      ),
      lon1 = c(
        s28434 = 57.78, Miami   = -80.226529,  Hawaii       = -155.467989
      ),
      lat2 = c(
        s28418 = 56.47, Bermuda =  32.294887,  NewZeland    =  -43.443219
      ),
      lon2 = c(
        s28418 = 53.73, Bermuda = -64.781380,  NewZeland    =  170.271360
      ),
      lat3 = c(
        point  = 57.00, SanJuan =  18.466319,  EasterIsland =  -27.112701
      ),
      lon3 = c(
        point  = 57.00, SanJuan = -66.105743,  EasterIsland = -109.349668
      )
    ),
    c(5170.969, 1147627.947, 28775528.873),
    tolerance = 1e-4
  )
})


test_that("*geointri* errs in calculation", {
  expect_true(
    geointri(
      lat1 = 56.47, lon1 = 53.73, lat2 = 58.02, lon2 = 56.30,
      lat3 = 56.65, lon3 = 57.78, lat = 57.000, lon = 57.000
    )
  )
  expect_true(
    !geointri(
      lat1 = 56.47, lon1 = 53.73, lat2 = 58.02, lon2 = 56.30,
      lat3 = 56.65, lon3 = 57.78, lat = 57.140, lon = 57.395
    )
  )
})
