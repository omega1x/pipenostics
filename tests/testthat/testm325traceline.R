library(pipenostics)

test_that("*m325traceline* errs in tracing regime parameters", {
  m325traceline_forward_report <- m325traceline(
    130, .588399, 250, seq(0, 30, 10), forward = TRUE, strict_sizes = TRUE
  )
  expect_equal(
    names(m325traceline_forward_report),
    c("temperature", "pressure", "flow_rate", "loss", "flux", "Q")
  )
  expect_equal(
    m325traceline_forward_report[["temperature"]],
    c(129.16981, 128.40753, 127.93775, 127.30389),
    tolerance = 1e-4
  )
  expect_equal(
    m325traceline_forward_report[["pressure"]],
    c(0.58779766, 0.58730839, 0.58707571, 0.58687323),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_forward_report[["flow_rate"]],
    c(250, 240, 220, 190)
  )
  expect_equal(
    m325traceline_forward_report[["loss"]],
    c(352.29000, 351.40917, 350.60039, 350.10195),
    tolerance = 1e-4
  )
  expect_equal(
    m325traceline_forward_report[["flux"]],
    c(183.42585702, 182.96723522, 182.54613218, 182.28661038),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_forward_report[["Q"]],
    c(5072976, 4469924.58163786, 2524322.80394798, 2940856.36847728),
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
    c(130.028177269546, 129.199677143122, 128.438826847396, 127.969571729179),
    tolerance = 1e-4
  )
  expect_equal(
    m325traceline_backward_report[["pressure"]],
    c(
      0.588559787214791, 0.58795799303854, 0.587468380652964, 0.587235594628342
    ),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_backward_report[["flow_rate"]],
    c(250, 250, 240, 220)
  )
  expect_equal(
    m325traceline_backward_report[["loss"]],
    c(351.440857448852, 350.633595285087, 350.135715604659, 349.4642387),
    tolerance = 1e-7
  )
  expect_equal(
    m325traceline_backward_report[["flux"]],
    c(182.983736324139, 182.563421372738, 182.304191740686, 181.954575723466),
    tolerance = 1e-4
  )
  expect_equal(
    m325traceline_backward_report[["Q"]],
    c(5060748.34726347, 4460059.3320263, 2520977.15235354, 2935499.60508),
    tolerance = 1e-1
  )
})
