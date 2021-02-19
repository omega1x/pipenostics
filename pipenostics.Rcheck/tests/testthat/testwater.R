library(pipenostics)

test_that("water's function errs in water states paths", {
  expect_equal(
    ps_t(),
    c(.353658941e-2, .263889776e1, .123443146e2),
    tolerance = 1e-8
  )

  expect_equal(
    cp1_tp(),
    c(.417301218, .401008987, 0.465580682)*10,
    tolerance = 1e-8
  )

  expect_equal(
    v1_tp(),
    c(.100215168e-2, .971180894e-3, .120241800e-2),
    tolerance = 1e-8
  )

  expect_equal(
    fric_romeo(2118517, c(0, 70e-3/1, 7e-3/1)),
    fric_buzelli(2118517, c(0, 70e-3/1, 7e-3/1)),
    tolerance = 1e-3
  )

  expect_equal(
    fric_romeo(2118517, c(0, 70e-3/1, 7e-3/1)),
    fric_vatankhan(2118517, c(0, 70e-3/1, 7e-3/1)),
    tolerance = 1e-3
  )

  expect_equal(
    all(
      fric_romeo(2118517, c(0, 70e-3/1, 7e-3/1)) < .2 &
      fric_romeo(2118517, c(0, 70e-3/1, 7e-3/1)) > 0
    ),
    TRUE
  )


  with(pipenostics:::r12t4, {
    expect_equal(
      dynvisc(temperature, density),
      dynvisc,
      tolerance = 1e-8
    )
  })

})
