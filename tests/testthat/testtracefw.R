library(pipenostics)

dhn <- list(
  sender = c(
    "N04", "N04", "N05", "N08", "N06",
    "N08", "N06", "N11", "N06", "N11", "N13", "N00", "N12", "N13",
    "N14", "N13", "N16", "N16", "N20", "N18", "N20", "N18", "N20",
    "N22", "N22", "N23"
  ),
  acceptor = c(
    "N01", "N02", "N03", "N04",
    "N05", "N06", "N07", "N08", "N09", "N10", "N11", "N12", "N13",
    "N14", "N15", "N16", "N17", "N18", "N19", "N20", "N21", "N22",
    "N23", "N24", "N25", "N26"
  ),
  temperature = c(
    69.3, 69.4, 68.6,
    NA, NA, NA, 70, NA, 69.2, 71.3, NA, 70.4942576977863, NA, NA,
    70, NA, 71.4, NA, 69.3, NA, 68.8, NA, NA, 69.2, 69.3, 68.5
  ),
  pressure = c(
    0.588399, 0.588399, 0.588399, NA, NA, NA, 0.588399,
    NA, 0.588399, 0.588399, NA, 0.613560201407145, NA, NA, 0.588399,
    NA, 0.588399, NA, 0.588399, NA, 0.588399, NA, NA, 0.588399,
    0.588399, 0.588399
  ),
  flow_rate = c(
    30, 30, 16, NA, NA, NA,
    20, NA, 16, 10, NA, 274, NA, NA, 30, NA, 10, NA, 16, NA,
    20, NA, NA, 30, 30, 16
  ),
  a = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0),
  d = c(
    150, 150, 80, 200, 80, 150,
    100, 200, 80, 50, 200, 300, 300, 150, 150, 200, 50, 200,
    80, 150, 100, 200, 80, 150, 150, 80
  ),
  wth = c(7, 7, 6, 8, 6, 7, 6, 8, 6, 6, 8, 19, 19, 7, 7, 8, 6, 8, 6,
          7, 6, 8, 6, 7, 7, 6),
  len = c(
    39.845, 39.845,
    77.274, 14.275, 36.168, 60.483, 72.446, 30, 30.079, 21.893,
    30, 89.522, 98.782, 68.32, 79.54, 30, 21.893, 30.81, 51.972,
    66.762, 72.446, 30.81, 51.972, 25.455, 25.455, 77.274
  ),
  year = c(
    1986L,
    1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L,
    1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L,
    1986L, 1986L, 1986L, 1986L, 1986L, 1986L, 1986L
  ),
  insulation = c(
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 2L, 1L, 1L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
  ),
  laying = c(
    "channel", "channel", "channel", "channel", "channel", "channel", "tunnel",
    "channel", "room", "room", "channel", "underground", "underground",
    "channel", "channel", "channel", "room", "channel", "room",
    "channel", "tunnel", "channel", "channel", "channel", "channel",
    "channel"
  ),
  beta = c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE
  ),
  exp5k = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  ),
  roughness = c(
    0.0015, 0.0015, 8e-04, 0.002, 8e-04, 0.0015,
    0.001, 0.002, 8e-04, 5e-04, 0.002, 0.003, 0.003, 0.0015,
    0.0015, 0.002, 5e-04, 0.002, 8e-04, 0.0015, 0.001, 0.002,
    8e-04, 0.0015, 0.0015, 8e-04), inlet = c(0, 0, 0.5, 0, 0.3,
    0, 0.5, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0.5
  ),
  outlet = c(
    0, 0, 0.5, 0, 0.5, 0.3, 1, 0, 0.5,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0.5
  )
)

m325_tracefw_ensample <- do.call(
  "m325tracefw", c(dhn, verbose = FALSE, elev_tol = .5)
)

dhn[c("a", "year", "insulation", "laying", "beta", "exp5k")] <- NULL
n <- length(dhn[["sender"]])

root_node <- 12
dhn[["temperature"]] <- append(
  rep.int(NA_real_, n - 1), 70.4942576978, root_node - 1
)
dhn[["pressure"]]    <- append(
  rep.int(NA_real_, n - 1), 0.6135602014, root_node - 1
)
dhn[["flow_rate"]]   <- append(
  rep.int(NA_real_, n - 1), 274.0, root_node - 1
)


actual_loss <- m325_tracefw_ensample[
  order(m325_tracefw_ensample[["node"]]), "loss"
]
actual_loss[[root_node]] <- 0

test_that("*tracefw* errs in calculation without execution parallelization", {
  tracefw_report <- do.call(
    "tracefw",
    c(as.list(dhn), list(loss = actual_loss), verbose = FALSE, elev_tol = .5)
  )

  expect_equal(
    all(colnames(tracefw_report) == colnames(m325_tracefw_ensample)),
    TRUE
  )

  expect_equal(
    tracefw_report[["temperature"]], m325_tracefw_ensample[["temperature"]]
  )

  expect_equal(
    tracefw_report[["pressure"]], m325_tracefw_ensample[["pressure"]]
  )

  expect_equal(
    tracefw_report[["flow_rate"]], m325_tracefw_ensample[["flow_rate"]]
  )

  expect_equal(
    tracefw_report[["loss"]], m325_tracefw_ensample[["loss"]]
  )
  rm(tracefw_report)
})

test_that(
  "*tracefw* errs in calculation utilizing parallel execution (if possible)", {
  tracefw_report <- do.call(
    "tracefw",
    c(
      as.list(dhn),
      list(
        loss = actual_loss,
        use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
      ),
      verbose = FALSE, elev_tol = .5
    )
  )

  expect_equal(
    all(colnames(tracefw_report) == colnames(m325_tracefw_ensample)),
    TRUE
  )

  expect_equal(
    tracefw_report[["temperature"]], m325_tracefw_ensample[["temperature"]]
  )

  expect_equal(
    tracefw_report[["pressure"]], m325_tracefw_ensample[["pressure"]]
  )

  expect_equal(
    tracefw_report[["flow_rate"]], m325_tracefw_ensample[["flow_rate"]]
  )

  expect_equal(
    tracefw_report[["loss"]], m325_tracefw_ensample[["loss"]]
  )
  rm(tracefw_report)
})

rm(actual_loss, root_node, n, m325_tracefw_ensample, dhn)

test_that("*tracefw* does not write csv-file", {
  file_name <- tempfile()
  tracefw(csv = TRUE, file = file_name)
  expect_equal(
    file.exists(file_name),
    TRUE
  )
  unlink(file_name)
})
