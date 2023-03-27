test_that("`validate.logical` works", {
  expect_equal(validate.logical(TRUE), TRUE)
  expect_equal(validate.logical(FALSE), TRUE)
  expect_equal(validate.logical(obj = TRUE), TRUE)
  expect_equal(validate.logical(obj = FALSE), TRUE)
  expect_equal(validate.logical(TRUE, single = FALSE), TRUE)
  expect_equal(validate.logical(TRUE, single = TRUE), TRUE)
  expect_warning(validate.logical(c(TRUE, FALSE), single = TRUE))
})
