library(pipenostics)

test_that("*m325traceline* errs in tracing regime parameters", {
  m325traceline_forward_report <- m325traceline(
    130, .588399, 250, seq(0, 30, 10), forward = TRUE
  )
  expect_equal(
    names(m325traceline_forward_report),
    c("temperature", "pressure", "flow_rate", "loss", "flux", "Q")
  )
  expect_equal(
   m325traceline_forward_report[["temperature"]],
   c(129.1799, 128.4269, 127.9628, 127.3367),
   tolerance = 1e-4
  )
  expect_equal(
    m325traceline_forward_report[["pressure"]],
    c(0.5877508, 0.5872233, 0.5869725, 0.5867542),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_forward_report[["flow_rate"]],
    c(250, 240, 220, 190)
  )
  expect_equal(
    m325traceline_forward_report[["loss"]],
    c(348, 347.1389, 346.3483, 345.8610),
    tolerance = 1e-4
  )
  expect_equal(
    m325traceline_forward_report[["flux"]],
    c(184.0395, 183.5841, 183.1660, 182.9083),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_forward_report[["Q"]],
    c(5011200, 4415607.97, 2493707.41, 2905232.11),
    tolerance = 1e-7
  )

  m325traceline_backward_report <- m325traceline(
    127.3367, .5870330, 190, seq(0, 30, 10), forward = FALSE
  )
  expect_equal(
    names(m325traceline_backward_report),
    c("temperature", "pressure", "flow_rate", "loss", "flux", "Q")
  )
  expect_equal(
    m325traceline_backward_report[["temperature"]],
    c(129.9953, 129.1769, 128.4254, 127.9619),
    tolerance = 1e-4
  )
  expect_equal(
    m325traceline_backward_report[["pressure"]],
    c(0.5886788, 0.5880301, 0.5875023, 0.5872514),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_backward_report[["flow_rate"]],
    c(250, 250, 240, 220)
  )
  expect_equal(
    m325traceline_backward_report[["loss"]],
    c(347.135781269, 346.346651110, 345.859948477, 345.203535),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_backward_report[["flux"]],
    c(183.5825, 183.1651, 182.9077, 182.5606),
    tolerance = 1e-4
  )
  expect_equal(
    m325traceline_backward_report[["Q"]],
    c(4998755.25, 4405529.40, 2490192.63, 2899710.69),
    tolerance = 1e-1
  )
})
