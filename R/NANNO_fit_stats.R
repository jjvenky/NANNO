#' NANNO Statistics Function
#'
#' This function takes model simulations and observations and calculates fitting statistics.
#' @param ysim The NANNO model simulation results.
#' @param yobs The field observations.
#' @keywords NANNO
#' @export
#' @examples
#' NANNO_fit_stats(ysim, yobs)


NANNO_fit_stats <- function (ysim, yobs) {
  data.frame(NO3 = c(cor(Y$NO3, yobs$NO3, use = "complete.obs", method = "pearson")^2,
                     sqrt(mean((Y$NO3 - yobs$NO3)^2, na.rm = TRUE)),
                     sum(!is.na(yobs$NO3))),
             TAN = c(cor(Y$TAN, yobs$TAN, use = "complete.obs", method = "pearson")^2,
                     sqrt(mean((Y$TAN - yobs$TAN)^2, na.rm = TRUE)),
                     sum(!is.na(yobs$TAN))),
             deltaNO3 = c(cor(Y$deltaNO3, yobs$deltaNO3, use = "complete.obs", method = "pearson")^2,
                          sqrt(mean((Y$deltaNO3 - yobs$deltaNO3)^2, na.rm = TRUE)),
                          sum(!is.na(yobs$deltaNO3))),
             deltaTAN = c(cor(Y$deltaTAN, yobs$deltaTAN, use = "complete.obs", method = "pearson")^2,
                          sqrt(mean((Y$deltaTAN - yobs$deltaTAN)^2, na.rm = TRUE)),
                          sum(!is.na(yobs$deltaTAN))),
             row.names = c("R2", "RMSE", "n"))
}

