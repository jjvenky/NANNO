#' Residuals Figure Function
#'
#' Takes model output and observations, calculates the residuals, and plots
#' them for each measured variable.
#'
#' @param ysim The NANNO model simulation results.
#' @param yobs The field observations (with \code{deltaTAN} and
#'   \code{deltaNO3} columns, as returned by \code{calcDeltas}).
#' @param obstime The field observation times (distances).
#' @return A ggplot object of residuals against distance.
#' @export
#' @examples
#' ysim <- calcDeltas(simecol::out(simecol::sim(NANNO_model())))
#' yobs <- calcDeltas(read.csv(system.file("extdata", "NANNO_example-data.csv",
#'                                         package = "NANNO")))
#' residuals_figures(ysim, yobs, yobs$X)
residuals_figures <- function(ysim, yobs, obstime) {
  a <- ysim[match(obstime, ysim$time),
            c("time", "NO3", "TAN", "deltaNO3", "deltaTAN")]
  names(a) <- c("time", "NO[3]^-{}", "TAN",
                "delta^{15}*N-NO[3]^-{}", "delta^{15}*N-TAN")
  b <- cbind(time = obstime, yobs)[, c("time", "NO3", "TAN", "deltaNO3", "deltaTAN")]
  names(b) <- c("time", "NO[3]^-{}", "TAN",
                "delta^{15}*N-NO[3]^-{}", "delta^{15}*N-TAN")

  a.melt <- tidyr::pivot_longer(a, cols = -time,
                                names_to = "variable", values_to = "obs_value")
  b.melt <- tidyr::pivot_longer(b, cols = -time,
                                names_to = "variable", values_to = "pred_value")
  d <- merge(as.data.frame(a.melt), as.data.frame(b.melt),
             by = c("time", "variable"))
  d$resid <- d$obs_value - d$pred_value

  ggplot2::ggplot(d, ggplot2::aes(x = time, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(~variable, scales = "free", labeller = "label_parsed") +
    ggplot2::labs(x = "Distance (m)", y = "Residual (mgN/L or \u2030 vs air)")
}
