library(pipenostics)

test_that("*b36mass* errs mass calculations when processing origins 1, 2", {
  ensample <- b36pipedata[b36pipedata[["origin"]] %in% c(1L, 2L), ]
  expect_equal(
    b36mass(ensample[["d"]], ensample[["wth"]], rho = 7.85, origin = NULL),
    ensample[["mass"]],
    tolerance = 0.01
  )
  rm(ensample)
})

C <- data.frame(
  a = c(
    1.34639486196096e-05,  -0.269421768471856, -0.543828305826526,
    -0.32040873191324,     -0.001023302221079,  0.000750308148511,
    -8.88387684950809e-05
  ),
  b = c(
    0.99993868472687, 1.06490339805795,  1.07025471029979,
    1.04765386422189, 1.00035194161928,  0.999990376939387,
    1.00000056550023
  )
)

for (i in unique(b36pipedata[["origin"]])) {
  ensample <- b36pipedata[b36pipedata[["origin"]] == i, ]

  test_that(
    sprintf(
      "*b36mass* errs mass calculations when processing origin [%i] in knots", i
    ), {
      output <- b36mass(
        ensample[["d"]], ensample[["wth"]], rho = ensample[["rho"]],
        origin = as.integer(i)
      )
      expect_equal(output, ensample[["mass"]])
    }
  )

  test_that(
    sprintf(
      paste(
        "*b36mass* errs mass calculations when processing",
        "origin [%i] out of knots"
      ), i
    ), {
      ensample[["d"]]  <- ensample[["d"]] + 0.1
      ensample[["mpe"]] <- {
        (ensample[["d"]] - ensample[["wth"]]) * 1e-3 * ensample[["wth"]] *
        ensample[["rho"]] * 1 * base::pi
      }
      ensample[["mass_restored"]] <- ensample[["mpe"]] * C[i, "b"] + C[i, "a"]
      output   <- b36mass(
        ensample[["d"]], ensample[["wth"]], rho = ensample[["rho"]],
        origin = as.integer(i)
      )
      expect_equal(output, ensample[["mass_restored"]])
    }
  )
}
rm(ensample, i)

test_that("*b36mass* errs in producing NAs for special cases", {
  ensample <- b36pipedata[
    b36pipedata[["origin"]] %in% c(8, 9) & b36pipedata[["d"]] < 25,
  ]
  output <- b36mass(
    ensample[["d"]], ensample[["wth"]], origin = as.integer(c(6, 7))
  )
  expect_equal(all(is.na(output)), TRUE)
  rm(output, ensample)
})

test_that("*b36d* errs in diameter calculations", {
  ensample <- b36pipedata[b36pipedata[["origin"]] %in% c(1L, 2L, 4L, 5L), ]
  output   <- b36d(ensample[["wth"]], ensample[["mass"]], rho = 7.85)
  r <- abs(output - ensample[["d"]]) / ensample[["d"]] * 100  # [%]
  expect_equal(all(r < 1), TRUE)
  rm(r, output, ensample)
})

test_that("*b36wth* errs in wall thickness calculations", {
  ensample <- b36pipedata[b36pipedata[["origin"]] %in% c(1L, 2L, 4L, 5L), ]
  output   <- b36wth(ensample[["d"]], ensample[["mass"]], rho = 7.85)
  expect_equal(median(abs(ensample[["wth"]] - output)) < 5e-4, TRUE)
  rm(ensample, output)
})

test_that("*b36dwthv* errs in diameter/wall thickness pair validation", {
  dwth_pair <- rbind(
    data.frame(
      d = c(
        351, 88.9, 325, 194, 140, 21, 56, 76, 530, 45, 57, 426, 56, 190, 60, 22,
        50, 75, 32, 32, 53, 355.6, 63, 130, 51
      ),
      wth = c(
        36, 7.14, 10, 13, 4.5, 6, 11, 1.4, 7, 12, 9.5, 13, 1.2, 14, 9, 3.5, 6,
        4, 6, 2.5, 8.5, 22.23, 5.5, 14, 5
      ),
      status = TRUE
    ),
    data.frame(
      d = c(351, 530, 153, 104, 190, 190), wth = c(37, 35, 9.5, 9.5, 3.1,  13),
      status = FALSE
    )
  )
  expect_equal(
    all(b36dwthv(dwth_pair[["d"]], dwth_pair[["wth"]]) == dwth_pair[["status"]])
    ,
    TRUE
  )
  rm(dwth_pair)
})
