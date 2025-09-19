library(pipenostics)

test_that(
  "*m325tracefw* errs in calculation without execution parallelization", {
  m325tracefw_report <- m325tracefw(verbose = FALSE, strict_sizes = TRUE)
  expect_equal(
    m325tracefw_report[["node"]],
    c("1", "2")
  )
  expect_equal(
    m325tracefw_report[["tracing"]],
    c("sensor", "1")
  )
  expect_equal(
    m325tracefw_report[["backward"]],
    c(FALSE, FALSE)
  )
  expect_equal(
    m325tracefw_report[["aggregation"]],
    c("identity", "identity")
  )
  expect_equal(
    m325tracefw_report[["temperature"]],
    c(70, 69.71603),
    tolerance = 1e-5
  )
  expect_equal(
    m325tracefw_report[["pressure"]],
    c(.588399007, 0.557794259),
    tolerance = 1e-5
  )
  expect_equal(
    m325tracefw_report[["flow_rate"]],
    c(20, 20)
  )
  expect_equal(
    m325tracefw_report[2, "loss"],
    78.4
  )
  expect_equal(
    m325tracefw_report[2, "flux"],
    290.23241,
    tolerance = 1e-3
  )
  expect_equal(
    m325tracefw_report[2, "Q"],
    136314.3936,
    tolerance = 1e-1
  )
  expect_equal(
    m325tracefw_report[["job"]],
    c(0, 1)
  )
})

test_that("*m325tracefw* errs in calculation utilizing parallel execution", {
  m325tracefw_report <- m325tracefw(
    verbose = FALSE,
    use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  )
  expect_equal(
    m325tracefw_report[["node"]],
    c("1", "2")
  )
  expect_equal(
    m325tracefw_report[["tracing"]],
    c("sensor", "1")
  )
  expect_equal(
    m325tracefw_report[["backward"]],
    c(FALSE, FALSE)
  )
  expect_equal(
    m325tracefw_report[["aggregation"]],
    c("identity", "identity")
  )
  expect_equal(
    m325tracefw_report[["temperature"]],
    c(70, 69.71603),
    tolerance = 1e-5
  )
  expect_equal(
    m325tracefw_report[["pressure"]],
    c(.588399007, 0.557794259),
    tolerance = 1e-5
  )
  expect_equal(
    m325tracefw_report[["flow_rate"]],
    c(20, 20)
  )
  expect_equal(
    m325tracefw_report[2, "loss"],
    78.4
  )
  expect_equal(
    m325tracefw_report[2, "flux"],
    290.23241,
    tolerance = 1e-3
  )
  expect_equal(
    m325tracefw_report[2, "Q"],
    136314.3936,
    tolerance = 1e-1
  )
  expect_equal(
    m325tracefw_report[["job"]],
    c(0, 1)
  )
})

test_that("*m325tracefw* does not write csv-file", {
  file_name <- tempfile()
  m325tracefw(csv = TRUE, file = file_name)
  expect_equal(
    file.exists(file_name),
    TRUE
  )
  unlink(file_name)
})
