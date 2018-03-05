#' NANNO Inputs Function
#'
#' This function takes inputs for lower and upper bounds from a csv file and converts them for the fitting routine
#' @param NANNOmodel S4 object of the NANNO model with fitted parameters (Formal class odeModel)
#' @param filename The base name of the bounds file to be used.
#' @keywords NANNO
#' @export
#' @examples
#' recreate_NANNO_fit(NANNOmodel, filename)

## Recreate fit from the model object
## 1. Need model object from NANNO_fit and file name of field data
## 2. Need ysim, yobs, obstime
## 3. Make two figures and calculate SSE

recreate_NANNO_fit <- function (NANNOmodel, filename) {

  # Model results
  ysim <- out(sim(NANNOmodel))

  # Load field data
  yobs <- read.csv(paste(paste(filename, "data", sep = "-"), "csv", sep = "."))

  # Prepare data
  yobs <- calcIsos(yobs)
  obstime <- yobs$X
  yobs <- subset(yobs, select = -c(X, deltaTAN, deltaNO3, deltaN2O, pH))
  ysim <- calcDeltas(ysim)
  yobs <- calcDeltas(yobs)

  # Figure
  two_part_figure_with_obs(ysim, yobs, obstime)

  # Figure
  residuals_figures(ysim, yobs, obstime)

  # Print SSE value on screen
  cat(c('SSE',
        as.numeric(ssqOdeModel(NULL, NANNOmodel, obstime, subset(yobs, select = c(TAN, NO3, N2O, isoTAN, isoNO3, isoN2O)))),
        '\n'))
}
