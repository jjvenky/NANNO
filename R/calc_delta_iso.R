#' Calculate Delta Values Function
#'
#' Takes model output or observations and calculates delta values from the
#' isotope pools.
#'
#' @param ysim A data frame of model output (or observations) with columns
#'   \code{TAN}, \code{NO3}, \code{isoTAN}, and \code{isoNO3}.
#' @return The input data frame with \code{DIN}, \code{deltaTAN},
#'   \code{deltaNO3}, \code{deltaN2O}, and \code{deltaDIN} columns added.
#' @export
#' @examples
#' ysim <- simecol::out(simecol::sim(NANNO_model()))
#' calcDeltas(ysim)
calcDeltas <- function(ysim) {
  ysim$DIN <- ysim$TAN + ysim$NO3
  ysim$deltaTAN <- ratioToDelta(ysim$isoTAN / ysim$TAN)
  ysim$deltaNO3 <- ratioToDelta(ysim$isoNO3 / ysim$NO3)
  if ("isoN2O" %in% names(ysim) && "N2O" %in% names(ysim)) {
    ysim$deltaN2O <- ratioToDelta(ysim$isoN2O / ysim$N2O)
  }
  ysim$deltaDIN <- (ysim$TAN * ysim$deltaTAN +
                      ysim$NO3 * ysim$deltaNO3) / (ysim$TAN + ysim$NO3)
  ysim
}

#' Calculate Iso Values Function
#'
#' Takes observations and calculates isotope pools from the delta values.
#'
#' @param yobs A data frame of observations with columns \code{TAN},
#'   \code{NO3}, \code{deltaTAN}, and \code{deltaNO3}.
#' @return The input data frame with \code{isoTAN}, \code{isoNO3}, and
#'   (where present) \code{isoN2O} columns added.
#' @export
#' @examples
#' yobs <- read.csv(system.file("extdata", "NANNO_example-data.csv",
#'                              package = "NANNO"), check.names = FALSE)
#' calcIsos(yobs)
calcIsos <- function(yobs) {
  yobs$isoTAN <- yobs$TAN * deltaToRatio(yobs$deltaTAN)
  yobs$isoNO3 <- yobs$NO3 * deltaToRatio(yobs$deltaNO3)
  if ("N2O" %in% names(yobs) && "deltaN2O" %in% names(yobs)) {
    yobs$isoN2O <- yobs$N2O * deltaToRatio(yobs$deltaN2O)
  }
  yobs
}
