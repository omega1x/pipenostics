library(pipenostics)

test_that("*m325traceline* errs in tracing regime parameters", {
  m325traceline_foward_report <- m325traceline(130, .588399, 250, seq(0, 30, 10), forward = TRUE)
  expect_equal(
    names(m325traceline_foward_report),
    c("temperature", "pressure", "flow_rate", "loss", "flux", "Q")
  )
  expect_equal(
   m325traceline_foward_report[["temperature"]],
   c(129.1799, 128.4269, 127.9628, 127.3367),
   tolerance = 1e-4
  )
  expect_equal(
    m325traceline_foward_report[["pressure"]],
    c(0.5878607, 0.5874226, 0.5872143, 0.5870330),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_foward_report[["flow_rate"]],
    c(250, 240, 220, 190)
  )
  expect_equal(
    m325traceline_foward_report[["loss"]],
    c(348, 347.138912477, 346.348251588, 345.860965187),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_foward_report[["flux"]],
    c(181.959958158, 181.509718360, 181.096302779, 180.841513660),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_foward_report[["Q"]],
    c(5011200, 4415607.97, 2493707.41, 2905232.11),
    tolerance = 1e-1
  )

  m325traceline_backward_report <- m325traceline(127.3367, .5870330, 190, seq(0, 30, 10), forward = FALSE)
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
    c(0.5883998, 0.5878611, 0.5874228, 0.5872144),
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
    c(181.508081135, 181.095465931, 180.840982050, 180.497760875),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_backward_report[["Q"]],
    c(4998755.25, 4405529.40, 2490192.63, 2899710.69),
    tolerance = 1e-1
  )
})
