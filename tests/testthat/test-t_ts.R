"it has a happy path" |>
test_that({

  # stub the call to Sys.time with a fake value
  mockery::stub(t_ts, "Sys.time", "2023-11-03 23:15:10 GMT")

  expect_equal(t_ts(), "20231103_231510")
})

