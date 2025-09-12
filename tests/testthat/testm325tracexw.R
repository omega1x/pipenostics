library(pipenostics)

test_that("*m325tracefw* and *m325tracebw* have the same colnames", {
  expect_equal(
    all(colnames(m325tracefw()) == colnames(m325tracebw())),
    TRUE
  )
})
