library(pipenostics)

test_that("*m325nhl* errs in normative heat losses", {
  expect_equal(
    m325nhl(1980, "underground", d = 73, temperature = 65, beta = c(FALSE, TRUE)),
    c(65.500, 75.325),
    tolerance = 1e-3
  )
  expect_equal(
    m325nhl(2000, "channel", d = 73, temperature = 65, beta = c(FALSE, TRUE)),
    c(17.533, 21.040),
    tolerance = 1e-3
  )

  with(m325nhldata, {
    expect_equal(
      m325nhl(epoch, laying, exp5k, insulation, as.double(diameter), temperature),
      flux
    )
  })
})


test_that("*m325traceline* errs in tracing regime parameters", {
  regime_fw <- m325traceline(130, .588399,250, seq(0, 30, 10), forward = TRUE)
  expect_equal(
    names(regime_fw),
    c("temperature", "pressure", "consumption")
  )
  expect_equal(
   regime_fw[["temperature"]],
   c(129.1799, 128.4269, 127.9628, 127.3367),
   tolerance = 1e-4
  )
  expect_equal(
    regime_fw[["pressure"]],
    c(0.5878607, 0.5874226, 0.5872143, 0.5870330),
    tolerance = 1e-7
  )
  expect_equal(
    regime_fw[["consumption"]],
    c(250, 240, 220, 190)
  )

  regime_bw <- m325traceline(127.3367, .5870330, 190, seq(0, 30, 10), forward = FALSE)
  expect_equal(
    names(regime_bw),
    c("temperature", "pressure", "consumption")
  )
  expect_equal(
    regime_bw[["temperature"]],
    c(129.9953, 129.1769, 128.4254, 127.9619),
    tolerance = 1e-4
  )
  expect_equal(
    regime_bw[["pressure"]],
    c(0.5883998, 0.5878611, 0.5874228, 0.5872144),
    tolerance = 1e-7
  )
  expect_equal(
    regime_bw[["consumption"]],
    c(250, 250, 240, 220)
  )
})


nx <- pipenostics::m325testbench
nx[["d"]] <- 1e3*nx[["d"]]  # convert [m] to [mm]
output <- do.call("m325tracebw", nx)

test_that("*m325tracebw* errs in losses", {
  with(subset(nx, acceptor == 2), {
    expect_equal(
      temperature +
      m325dropt(
        temperature, pressure, consumption,
        d, len, year, insulation, laying, beta, exp5k
      ),
      subset(output, node == 4 & trace == 2 & aggregation == "identity",
             "temperature")[[1]]
    )

    expect_equal(
        pressure +
        dropp(
          temperature, pressure, consumption,
          d*1e-3, len, roughness, inlet, outlet, "romeo"
        ),
        subset(output, node == 4 & trace == 2 & aggregation == "identity",
               "pressure")[[1]]
    )
  })
})


test_that("*m325tracebw* errs in balance calculation", {
  expect_equal(
    subset(
       output, node == 6 & aggregation == "median", "consumption"
    )[1, ],
    unname(colSums(subset(nx, acceptor %in% c(3, 7, 9), "consumption")))
  )
})

test_that("*m325tracebw* does not write csv-file", {
  file_name <- tempfile()
  m325tracebw(csv = TRUE, file = file_name)
  expect_equal(
    file.exists(file_name),
    TRUE
  )
  unlink(file_name)
})

test_that("*m325tracefw* errs in calculation", {
  skip_on_cran()
  result <- m325tracefw(verbose = FALSE)
  expect_equal(
    result[["node"]],
    c("1", "2")
  )
  expect_equal(
    result[["trace"]],
    c("sensor", "1")
  )
  expect_equal(
    result[["backward"]],
    c(FALSE, FALSE)
  )
  expect_equal(
    result[["aggregation"]],
    c("identity", "identity")
  )
  expect_equal(
    result[["temperature"]],
    c(70, 69.71603),
    tolerance = 1e-5
  )
  expect_equal(
    result[["pressure"]],
    c(.5883990, 0.5813153)
  )
  expect_equal(
    result[["consumption"]],
    c(20, 20)
  )
  expect_equal(
    result[["job"]],
    c(0, 1)
  )


})


test_that("*m325tracefw* does not write csv-file", {
  skip_on_cran()
  file_name <- tempfile()
  m325tracefw(csv = TRUE, file = file_name)
  expect_equal(
    file.exists(file_name),
    TRUE
  )
  unlink(file_name)
})


# *m325dropt* is tesetd in testdrops
#

test_that("*m325beta* errs in calculation", {
  data(m325nhldata)
  expect_equal(
    unique(m325beta(m325nhldata[["laying"]], as.double(m325nhldata[["diameter"]]))),
    c(1.15, 1.2)
  )
})
