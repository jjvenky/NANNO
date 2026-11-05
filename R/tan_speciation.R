#' TAN Speciation Function
#'
#' Takes TAN concentration and pH and returns the concentrations of NH3 and
#' NH4+ as a named vector.
#'
#' @param TAN The TAN concentration.
#' @param pH The pH.
#' @return A named numeric vector with elements \code{NH3} and \code{NH4}.
#' @export
#' @examples
#' TAN_speciation(10, 8.5)
TAN_speciation <- function(TAN, pH) {
  fNH3 <- fnh3_from_pH(pH)
  fNH4 <- 1 - fNH3
  c(NH3 = TAN * fNH3,
    NH4 = TAN * fNH4)
}

#' TAN Isotope Speciation Function
#'
#' Takes TAN concentration, the TAN isotope pool (ratio times concentration),
#' and pH and returns the isotope pools of NH3 and NH4+ as a named vector.
#'
#' @param TAN The TAN concentration.
#' @param isoTAN The isoTAN value (TAN isotopic ratio times TAN concentration).
#' @param pH The pH.
#' @return A named numeric vector with elements \code{isoNH3} and \code{isoNH4}.
#' @export
#' @examples
#' TAN_isotope_speciation(10, 10 * deltaToRatio(10), 8.5)
TAN_isotope_speciation <- function(TAN, isoTAN, pH) {
  if (TAN <= 0) {
    return(c(isoNH3 = 0, isoNH4 = 0))
  }
  fNH3 <- fnh3_from_pH(pH)
  fNH4 <- 1 - fNH3
  NH3 <- TAN * fNH3
  NH4 <- TAN * fNH4
  aeq <- nanno_constants$alpha_eqm
  # Divide the NH3-NH4+ equilibrium fractionation between the two species,
  # convert back to a ratio, and multiply by the mass to get the isotope pool.
  c(isoNH3 = isoTAN * (1 - (1 - aeq) * fNH4) / TAN * NH3,
    isoNH4 = isoTAN / (1 - (1 - aeq) * fNH3) / TAN * NH4)
}
