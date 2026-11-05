#' NANNO Statistics Function
#'
#' Takes model output and observations and calculates fit statistics
#' (R-squared, RMSE, and the number of observations) for each measured
#' variable.
#'
#' @param ysim The NANNO model simulation results.
#' @param yobs The field observations.
#' @param obstime The field observation times (distances).
#' @return A data frame with rows \code{R2}, \code{RMSE}, and \code{n}.
#' @export
#' @examples
#' ysim <- calcDeltas(simecol::out(simecol::sim(NANNO_model())))
#' yobs <- calcIsos(read.csv(system.file("extdata", "NANNO_example-data.csv",
#'                                       package = "NANNO"), check.names = FALSE))
#' NANNO_fit_stats(ysim, yobs, yobs$X)
NANNO_fit_stats <- function(ysim, yobs, obstime) {
  # Select simulated values at the observation times by matching time values,
  # not by using obstime as row indices.
  Y <- ysim[match(obstime, ysim$time), , drop = FALSE]
  vars <- c("TAN", "NO3", "deltaTAN", "deltaNO3")
  stats <- lapply(vars, function(v) {
    x <- yobs[[v]]
    y <- Y[[v]]
    ok <- !is.na(x) & !is.na(y)
    r2 <- if (sum(ok) >= 2) cor(x[ok], y[ok])^2 else NA_real_
    c(R2 = r2,
      RMSE = sqrt(mean((y[ok] - x[ok])^2)),
      n = sum(ok))
  })
  out <- as.data.frame(stats, stringsAsFactors = FALSE)
  names(out) <- vars
  row.names(out) <- c("R2", "RMSE", "n")
  out
}
