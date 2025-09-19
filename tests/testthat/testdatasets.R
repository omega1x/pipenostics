library(pipenostics)

test_that("*m325nxdata* contains valid *b36pipedata* diameters and widths", {
  expect_true(all(b36dwthv(m325nxdata[["d"]], m325nxdata[["wth"]])))
})
