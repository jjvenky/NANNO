#' Calculate Masses For Each Process
#'
#' This function takes model results from a NANNOmodel via parms() and simulation values from ysim and calculates the mass of nitrogen moved by each process
#' @param NANNOmodel The NANNO model object, typically after model fitting.
#' @param ysim The NANNO model simulation results.
#' @keywords NANNO
#' @export
#' @examples
#' calcMasses(NANNOmodel, ysim)

calcMasses <- function(NANNOmodel, ysim) {
    masses <- data.frame(ge = sum(parms(NANNOmodel)["kge"] * ysim$NH3[-1]),
                         nit1 = sum(parms(NANNOmodel)["knit1"] * ysim$NH4[-1]),
                         nit2 = sum(parms(NANNOmodel)["knit2"] * ysim$NO2[-1]),
                         denit = sum(parms(NANNOmodel)["kdenit"] * ysim$NO3[-1]),
                         amup = sum(parms(NANNOmodel)["kamup"] * ysim$NH4[-1]))
    return(masses)
}
