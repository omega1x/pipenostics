library(pipenostics)

test_that("*flux_loss* errs in calculation", {
  expect_equal(
    flux_loss(c(218, 1040), c(998, 1395) * 1e-3, c(2, 5)),
    c(80.70238, 275.00155),
    tolerance = 5e-6
  )
})

test_that("*loss_flux* errs in calculation", {
  expect_equal(
    loss_flux(c(80.70238, 275.00155), c(998, 1395) * 1e-3, c(2, 5)),
    c(218, 1040),
    tolerance = 5e-6
  )
})

gost30732dwth <- data.frame(
  d = c(
    25,  32,    38,  45,  57,  76,  89,
    108,  114,  133, 159, 219, 273,
    325,  377,  426, 530, 630, 720, 820, 920,
    1020, 1220, 1420
  ),
  wth = c(
    2.5, 3, 3, 3, 3, 3, 4, 4, 4, 4,
    4.5, 6, 7, 7, 7, 7, 7, 8, 8, 9,
    10  , 11, 11, 12
  )
)

test_that("*wth_d* errs in calculation", {
  expect_equal(
    all(wth_d(gost30732dwth[["d"]]) - gost30732dwth[["wth"]] == 0),
    TRUE
  )
})

rm(gost30732dwth)
