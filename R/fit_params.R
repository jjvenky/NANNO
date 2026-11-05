#' NANNO Params Function
#'
#' Extracts the actively fitted parameter values from a fitted NANNO model
#' object.
#'
#' @param NANNOmodel The NANNO model object, typically after model fitting.
#' @param whichpar The names of the parameters that were actively fit.
#' @return A data frame with one column, \code{value}, and one row per
#'   parameter.
#' @export
#' @examples
#' NANNO_fit_params(NANNO_model(), c("kge", "knit1"))
NANNO_fit_params <- function(NANNOmodel, whichpar) {
  data.frame(value = format(parms(NANNOmodel)[whichpar],
                            digits = 3, scientific = FALSE),
             stringsAsFactors = FALSE)
}
