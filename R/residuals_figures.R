#' Two Part Figure with Observations Function
#'
#' This function takes model simulations and observations, calculate the residuals and plots them.
#' @param ysim The NANNO model simulation results.
#' @param yobs The field observations.
#' @param obstime Field observation times.
#' @keywords NANNO
#' @export
#' @examples
#' residuals_figures(ysim, yobs, obstime)

residuals_figures <- function(ysim, yobs, obstime) {
  a <- subset(ysim[match(obstime, ysim$time), ], select = c(time, NO3, TAN, deltaNO3, deltaTAN))
  names(a) <- c("time", "NO[3]^-{}", "TAN", "delta^{15}*N-NO[3]^-{}", "delta^{15}*N-TAN")
  b <- subset(cbind(time = obstime, yobs), select = c(time, NO3, TAN, deltaNO3, deltaTAN))
  names(b) <- c("time", "NO[3]^-{}", "TAN", "delta^{15}*N-NO[3]^-{}", "delta^{15}*N-TAN")
  d <- cbind(reshape2::melt(a, value.name = "obs_value", id.vars = "time"), reshape2::melt(b, value.name = "pred_value", id.vars = "time"))[, c(-4, -5)]
  d$resid <- d$obs_value - d$pred_value

  g.figure <- ggplot2::ggplot(d, ggplot2::aes(x = time, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(~variable, scales = "free", labeller = "label_parsed") +
    ggplot2::labs(x = "Distance (m)", y = "Residual (mgN/L or \u2030 vs air)")
  return(g.figure)
}
