library(pipenostics)

test_that("*traceline* errs in tracing regime parameters", {
  regime_fw <- traceline(
    130, .588399, 250, seq(0, 30, 10),
    loss = c(348, 347.1389, 346.3483, 345.8610), forward = TRUE
  )

  expect_equal(
    names(regime_fw),
    c("temperature", "pressure", "flow_rate", "loss", "flux", "Q")
  )
  expect_equal(
    regime_fw[["temperature"]],
    c(129.1799, 128.4269, 127.9628, 127.3367),
    tolerance = 1e-4
  )
  expect_equal(
    regime_fw[["pressure"]],
    c(0.587844, 0.587392, 0.587178, 0.586991),
    tolerance = 1e-5
  )
  expect_equal(
    regime_fw[["flow_rate"]],
    c(250, 240, 220, 190)
  )
  expect_equal(
    regime_fw[["loss"]],
    c(348, 347.138912477, 346.348251588, 345.860965187),
    tolerance = 1e-7
  )
  expect_equal(
    regime_fw[["flux"]],
    c(178.92729, 178.48455, 178.07806, 177.82751),
    tolerance = 1e-4
  )
  expect_equal(
    regime_fw[["Q"]],
    c(5011200, 4415607.97, 2493707.41, 2905232.11),
    tolerance = 1e-1
  )

  regime_bw <- traceline(
    127.3367, .5870330, 190, seq(0, 30, 10),
    loss = c(348, 347.1389, 346.3483, 345.8610), forward = FALSE
  )
  expect_equal(
    names(regime_bw),
    c("temperature", "pressure", "flow_rate", "loss", "flux", "Q")
  )
  expect_equal(
    regime_bw[["temperature"]],
    c(130.000893685, 129.180497939, 128.427226907, 127.963046346),
    tolerance = 1e-4
  )
  expect_equal(
    regime_bw[["pressure"]],
    c(0.5884422, 0.5878867, 0.5874348, 0.5872200),
    tolerance = 1e-6
  )
  expect_equal(
    regime_bw[["flow_rate"]],
    c(250, 250, 240, 220)
  )
  expect_equal(
    regime_bw[["loss"]],
    c(348, 347.1389, 346.3483, 345.8610),
    tolerance = 1e-7
  )
  expect_equal(
    regime_bw[["flux"]],
    c(178.9272922, 178.4845500, 178.0780560, 177.8275063),
    tolerance = 1e-7
  )
  expect_equal(
    regime_bw[["Q"]],
    c(5011200, 4415606.808, 2493707.76, 2905232.4),
    tolerance = 1e-3
  )
})
