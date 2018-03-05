#' Calculate Masses For Each Process
#'
#' This function takes model results from a NANNOmodel via parms() and simulation values from ysim and calculates the mass of nitrogen moved by each process
#' @param NANNOmodel The NANNO model object, typically after model fitting.
#' @param ysim The NANNO model simulation results.
#' @param obspH The observed pH.
#' @keywords NANNO
#' @export
#' @examples
#' NANNO_calc_masses(NANNOmodel, ysim, obspH)

NANNO_calc_masses <- function(NANNOmodel, ysim, pH) {
  fNH3 <- (10^-9.25/10^-pH)/(1+10^-9.25/10^-pH)
  fNH4 <- 1 - fNH3
  NH3 = as.numeric(ysim$TAN * fNH3)
  NH4 = as.numeric(ysim$TAN * fNH4)

  masses <- data.frame(ge = sum(parms(NANNOmodel)["kge"] * NH3[-1]),
                       amup = sum(parms(NANNOmodel)["kamup"] * NH4[-1]),
                       nit1 = sum(parms(NANNOmodel)["knit1"] * NH4[-1]),
                       denit = sum(parms(NANNOmodel)["kdenit"] * ysim$NO3[-1])
  )
  return(masses)
}
