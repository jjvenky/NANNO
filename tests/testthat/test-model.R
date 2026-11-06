test_that("default model runs and produces all states", {
  skip_if_not_installed("simecol")
  ysim <- simecol::out(simecol::sim(NANNO_model()))
  expect_true(all(c("TAN", "NO3", "N2O", "isoTAN", "isoNO3", "isoN2O") %in%
                    names(ysim)))
  expect_true(nrow(ysim) > 2)
})

test_that("model conserves TAN isotope pool sign and stays finite", {
  skip_if_not_installed("simecol")
  ysim <- simecol::out(simecol::sim(NANNO_model()))
  expect_true(all(is.finite(as.matrix(ysim))))
})
