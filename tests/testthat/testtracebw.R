library(pipenostics)

test_that("*tracebw* does not write csv-file", {
  file_name <- tempfile()
  tracebw(csv = TRUE, file = file_name)
  expect_equal(
    file.exists(file_name),
    TRUE
  )
  unlink(file_name)
})

DHN <- pipenostics::m325nxdata

DHN[["sender"]]   <- sprintf("N%02i", DHN[["sender"]])
DHN[["acceptor"]] <- sprintf("N%02i", DHN[["acceptor"]])

m325_tracebw_ensample <- do.call("m325tracebw", DHN)
m325_tracebw_ensample <- subset(
  m325_tracebw_ensample[
    order(m325_tracebw_ensample[["node"]]),
  ], aggregation == "median"
)
actual_loss <- m325_tracebw_ensample[["loss"]]

actual_loss <- c(
  96.7797507566675, 96.7797507566675,  71.1808264048755,     116.65263776791  ,
  71.2923787993057, 96.7931935872254,  78.5007768719699,     116.676286487434 ,
  28.6262356192016, 24.5482097144085, 116.698548270144 , 0, 153.175635850318 ,
  96.8283016183455, 96.7711148826053, 116.698548270144 ,      24.5482097144085,
  116.676286487434 , 28.6172734296851,  96.7927053107223,      78.4922751902151,
  116.651999252483 , 71.2845855910305,  96.7636915531738,      96.7636915531738,
  71.1243060409466
)

DHN[c("a", "year", "insulation", "laying", "beta", "exp5k")] <- NULL
tracebw_report <- do.call("tracebw", c(as.list(DHN), list(loss = actual_loss)))
tracebw_report <- subset(
  tracebw_report[
    order(tracebw_report[["node"]]),
  ], aggregation == "median"
)

test_that(
  "*tracebw* does not produce the form of ensample results", {

  expect_equal(
    all(colnames(tracebw_report) == colnames(m325_tracebw_ensample)), TRUE
  )
  expect_equal(
    all(tracebw_report[["node"]] == m325_tracebw_ensample[["node"]]), TRUE
  )
  expect_equal(
    all(tracebw_report[["tracing"]] == m325_tracebw_ensample[["tracing"]]),
    TRUE
  )
  expect_equal(
    all(tracebw_report[["backward"]] == m325_tracebw_ensample[["backward"]]),
    TRUE
  )
  expect_equal(
    all(
      tracebw_report[["aggregation"]] == m325_tracebw_ensample[["aggregation"]]
    ),
    TRUE
  )
  expect_equal(
    all(tracebw_report[["job"]] == m325_tracebw_ensample[["job"]]), TRUE
  )
})

test_that(
  "*tracebw* does not reproduce ensample values of *temperature*", {
  expect_equal(
    tracebw_report[["temperature"]], m325_tracebw_ensample[["temperature"]],
    tolerance = 0.1
  )
  }
)

test_that(
  "*tracebw* does not reproduce ensample values of *pressure*", {
  expect_equal(
    tracebw_report[["pressure"]], m325_tracebw_ensample[["pressure"]],
    tolerance = 1e-5
  )
  }
)

test_that(
  "*tracebw* does not reproduce ensample values of *flow_rate*", {
  expect_equal(
    tracebw_report[["flow_rate"]], m325_tracebw_ensample[["flow_rate"]],
    tolerance = 1e-4
  )
  }
)

#test_that(
#  "*tracebw* does not reproduce ensample values of *Q*", {
#  expect_equal(
#    tracebw_report[["Q"]], m325_tracebw_ensample[["Q"]], tolerance = 1e-4
#  )
#  }
#)

test_that(
  "*tracebw* does not reproduce ensample values of *loss*", {
  expect_equal(
    tracebw_report[["loss"]], m325_tracebw_ensample[["loss"]],
    tolerance = 1e-4
  )
  }
)

test_that(
  "*tracebw* does not reproduce ensample values of *flux*", {
  expect_equal(
    tracebw_report[["flux"]], m325_tracebw_ensample[["flux"]], tolerance = 1e-4
  )
  }
)
rm(tracebw_report, actual_loss, m325_tracebw_ensample, DHN)

