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

dhn <- pipenostics::m325nxdata

dhn[["sender"]]   <- sprintf("N%02i", dhn[["sender"]])
dhn[["acceptor"]] <- sprintf("N%02i", dhn[["acceptor"]])

m325_tracebw_ensample <- do.call("m325tracebw", dhn)
m325_tracebw_ensample <- subset(
  m325_tracebw_ensample[
    order(m325_tracebw_ensample[["node"]]),
  ], aggregation == "median"
)

# The next is also a result of `m325tracebw`-tracing:
actual_loss <- c(
  96.236, 96.288, 70.584, 116.044943125762, 70.7340165868372,
  96.2114863150603, 78.4, 116.015881619773, 28.1152, 24.9182, 116.679050351562,
  152.831147448797, 152.789332127695, 96.7331745004449, 96.6, 116.666828494072,
  24.9596, 115.922823255434, 28.1658, 96.1226072815915, 77.824,
  115.945514486784, 70.6899252508703, 96.184, 96.236, 70.54
)

dhn[c("a", "year", "insulation", "laying", "beta", "exp5k")] <- NULL
tracebw_report <- do.call("tracebw", c(as.list(dhn), list(loss = actual_loss)))
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

test_that("*tracebw* does not reproduce ensample values of *temperature*", {
  expect_equal(
    tracebw_report[["temperature"]], m325_tracebw_ensample[["temperature"]],
    tolerance = 0.1
  )
})

test_that("*tracebw* does not reproduce ensample values of *pressure*", {
  expect_equal(
    tracebw_report[["pressure"]], m325_tracebw_ensample[["pressure"]],
    tolerance = 1e-5
  )
})

test_that("*tracebw* does not reproduce ensample values of *flow_rate*", {
  expect_equal(
    tracebw_report[["flow_rate"]], m325_tracebw_ensample[["flow_rate"]],
    tolerance = 1e-4
  )
})

test_that("*tracebw* does not reproduce ensample values of *Q*", {
  expect_equal(
    tracebw_report[["Q"]], m325_tracebw_ensample[["Q"]], tolerance = 1e-4
  )
})

test_that("*tracebw* does not reproduce ensample values of *loss*", {
  expect_equal(
    tracebw_report[["loss"]], m325_tracebw_ensample[["loss"]],
    tolerance = 1e-4
  )
})

test_that("*tracebw* does not reproduce ensample values of *flux*", {
  expect_equal(
    tracebw_report[["flux"]], m325_tracebw_ensample[["flux"]], tolerance = 1e-4
  )
})

rm(tracebw_report, actual_loss, m325_tracebw_ensample, dhn)
