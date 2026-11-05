test_that("bounds file transposes to lower/upper/initial rows", {
  b <- NANNO_input_converting(system.file("extdata", "NANNO_example-bounds.csv",
                                          package = "NANNO"))
  expect_setequal(rownames(b), c("lower", "upper", "initial"))
  expect_true("kge" %in% colnames(b))
})

test_that("missing bounds column errors informatively", {
  skip_if_not_installed("simecol")
  tmp <- tempfile(fileext = ".csv")
  writeLines("variable,lower,upper\nkge,0.001,0.01", tmp)
  expect_error(NANNO_input_converting(tmp), "missing column")
})
