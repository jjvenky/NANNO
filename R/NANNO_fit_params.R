#' NANNO Params Function
#'
#' This function takes model simulations and observations and calculates fitting statistics.
#' @param NANNOmodel The NANNO model object, typically after model fitting.
#' @param whichpar The parameters that are actively fit.
#' @keywords NANNO
#' @export
#' @examples
#' NANNO_fit_params(NANNOmodel, whichpar)


NANNO_fit_params <- function (NANNOmodel, whichpar) {
  data.frame(value = format(parms(NANNOmodel)[whichpar], digits = 3, scientific = FALSE))
}

