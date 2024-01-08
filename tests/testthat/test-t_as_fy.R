"happy path" |>
  test_that({

    expect_equal(
      t_as_fy(as.Date("2020-03-31")),
      "2019/20"
      )

    expect_equal(
      t_as_fy(as.Date("2020-04-01")),
      "2020/21"
      )

  })

"it errors if anything other than date is passed in" |>
  test_that({

    expect_error(
      t_as_fy("not a date"),
      "The argument 'date' must be of class 'Date'."
      )

  })
