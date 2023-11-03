"it has a happy path" |>
test_that({

  a <- "a"
  b <- c("b", "c")

  expect_true(a %not_in% b)

  expect_false(a %in% b)

})

"it works with two vectors" |>
test_that({

  a <- c("a", "b")
  b <- c("b", "c")

  expect_equal(a %not_in% b, c(TRUE, FALSE))

  expect_equal(a %in% b, c(FALSE, TRUE))

})
