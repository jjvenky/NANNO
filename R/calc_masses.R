#' Calculate Masses For Each Process
#'
#' Takes a NANNO model object and its simulation output and calculates the
#' mass of nitrogen moved by each process. Sums assume unit simulation time
#' steps (one metre of travel distance per step here) and unit volume.
#'
#' @param NANNOmodel The NANNO model object, typically after model fitting.
#' @param ysim The NANNO model simulation results.
#' @param pH The pH used for TAN speciation.
#' @return A one-row data frame with the integrated process masses.
#' @export
#' @examples
#' tm1 <- NANNO_model()
#' ysim <- simecol::out(simecol::sim(tm1))
#' NANNO_calc_masses(tm1, ysim, 8.5)
NANNO_calc_masses <- function(NANNOmodel, ysim, pH) {
  fNH3 <- fnh3_from_pH(pH)
  fNH4 <- 1 - fNH3
  NH3 <- as.numeric(ysim$TAN * fNH3)
  NH4 <- as.numeric(ysim$TAN * fNH4)

  masses <- data.frame(ge = sum(parms(NANNOmodel)["kge"] * NH3[-1]),
                       amup = sum(parms(NANNOmodel)["kamup"] * NH4[-1]),
                       nit1 = sum(parms(NANNOmodel)["knit1"] * NH4[-1]),
                       denit = sum(parms(NANNOmodel)["kdenit"] * ysim$NO3[-1]))
  masses
}
