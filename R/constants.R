# Internal package constants for NANNO -------------------------------------
#
# Central definitions for the isotope standards and equilibrium chemistry so
# that every function reads the same values from one place. These objects are
# internal and deliberately not exported or roxygen-documented; see the
# individual function help pages for how each value is used.

nanno_constants <- list(
  r_std = 0.0036765,   # 15N/14N ratio of atmospheric N2 (AIR standard)
  pKa_NH4 = 9.25,      # pKa of NH4+ at 25 degrees C
  alpha_eqm = 0.955    # NH3-NH4+ equilibrium isotope fractionation factor
)

# Fraction of TAN present as NH3 for a given pH (internal helper, not exported)
fnh3_from_pH <- function(pH) {
  Ka <- 10^-nanno_constants$pKa_NH4
  denom <- Ka + 10^-pH
  Ka / denom
}
