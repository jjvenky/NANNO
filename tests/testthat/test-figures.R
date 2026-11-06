test_that("NANNO_test runs without error (regression: isoNO2 bug)", {
  skip_if_not_installed("simecol")
  # suppressWarnings: delta/permille glyph substitution warnings depend on the
  # plotting device and locale, not on package code.
  expect_no_error(suppressWarnings(NANNO_test()))
})

test_that("two_part_figure_with_obs runs (regression: rm.na bug)", {
  skip_if_not_installed("simecol")
  ysim <- calcDeltas(simecol::out(simecol::sim(NANNO_model())))
  yobs <- calcIsos(read.csv(system.file("extdata", "NANNO_example-data.csv",
                                        package = "NANNO")))
  yobs <- calcDeltas(yobs)
  expect_no_error(suppressWarnings(print(two_part_figure_with_obs(ysim, yobs, yobs$X))))
})

test_that("residuals figure and fit stats align on observation times", {
  skip_if_not_installed("simecol")
  ysim <- calcDeltas(simecol::out(simecol::sim(NANNO_model())))
  yobs <- calcDeltas(calcIsos(read.csv(
    system.file("extdata", "NANNO_example-data.csv", package = "NANNO"))))
  s <- NANNO_fit_stats(ysim, yobs, yobs$X)
  expect_true(all(c("R2", "RMSE", "n") %in% rownames(s)))
  expect_true(all(s["n", ] <= nrow(yobs)))
  expect_no_error(suppressWarnings(print(residuals_figures(ysim, yobs, yobs$X))))
})
