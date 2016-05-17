#' Calculate Delta Values Function
#'
#' This function takes model simulations and calculates delta values.
#' @param ysim The NANNO model simulation results.
#' @keywords NANNO
#' @export
#' @examples
#' calcDeltas(ysim)

calcDeltas <- function(ysim) {
    ysim$DIN <- ysim$TAN + ysim$NO2 + ysim$NO3
    ysim$deltaNH3 <- ratioToDelta(ysim$isoNH3/ysim$NH3)
    ysim$deltaNH4 <- ratioToDelta(ysim$isoNH4/ysim$NH4)
    ysim$deltaNO2 <- ratioToDelta(ysim$isoNO2/ysim$NO2)
    ysim$deltaNO3 <- ratioToDelta(ysim$isoNO3/ysim$NO3)
    ysim$deltaTAN <- ratioToDelta(ysim$isoTAN/ysim$TAN)
    ysim$deltaN2O <- ratioToDelta(ysim$isoN2O/ysim$N2O)
    ysim$deltaDIN <- (ysim$TAN * ysim$deltaTAN + ysim$NO2 * ysim$deltaNO2 + ysim$NO3 * ysim$deltaNO3)/(ysim$TAN + ysim$NO2 + ysim$NO3)
    return(ysim)
}



#' Calculate Iso Values Function
#'
#' This function takes model simulations and calculates delta values.
#' @param ysim The NANNO model simulation results.
#' @keywords NANNO
#' @export
#' @examples
#' calcDeltas(ysim)

calcIsos <- function(ysim) {
    ysim$isoNH3 <- ysim$NH3 * deltaToRatio(ysim$deltaNH3)
    ysim$isoNH4 <- ysim$NH4 * deltaToRatio(ysim$deltaNH4)
    ysim$isoNO2 <- ysim$NO2 * deltaToRatio(ysim$deltaNO2)
    ysim$isoNO3 <- ysim$NO3 * deltaToRatio(ysim$deltaNO3)
    ysim$isoTAN <- ysim$TAN * deltaToRatio(ysim$deltaTAN)
    ysim$isoN2O <- ysim$N2O * deltaToRatio(ysim$deltaN2O)
    return(ysim)
}
