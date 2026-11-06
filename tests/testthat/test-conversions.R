test_that("delta-ratio round trip is exact", {
  deltas <- c(-40, -20, 0, 5, 10, 40)
  expect_equal(ratioToDelta(deltaToRatio(deltas)), deltas)
})

test_that("AIR standard ratio is used", {
  expect_equal(ratioToDelta(0.0036765), 0)
  expect_equal(deltaToRatio(0), 0.0036765)
})

test_that("speciation fractions sum to one", {
  sp <- TAN_speciation(10, 8.5)
  expect_equal(unname(sum(sp)), 10)
  expect_true(sp["NH4"] > sp["NH3"])
})
