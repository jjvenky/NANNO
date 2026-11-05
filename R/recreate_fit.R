#' Recreate a NANNO Fit
#'
#' Re-runs a fitted NANNO model against the original field data and redraws
#' the fit and residual figures and the SSE, without refitting.
#'
#' @param NANNOmodel S4 \code{odeModel} object of the NANNO model, typically
#'   returned by \code{NANNO_fit()}.
#' @param filename The base name of the field data file (reads
#'   \code{<filename>-data.csv}), or the full path to the data CSV.
#' @return A list with the simulated data (\code{ysim}), observed data
#'   (\code{yobs}), and the sum of squares (\code{SSE}), invisibly.
#' @export
#' @examples
#' \dontrun{
#' recreate_NANNO_fit(NANNOmodel, "NANNO_example")
#' }
recreate_NANNO_fit <- function(NANNOmodel, filename) {
  ysim <- calcDeltas(out(sim(NANNOmodel)))

  data_file <- if (grepl("\\.csv$", filename)) {
    filename
  } else {
    paste(paste(filename, "data", sep = "-"), "csv", sep = ".")
  }
  if (!file.exists(data_file)) {
    stop("Field data file not found: ", data_file, call. = FALSE)
  }
  yobs <- read.csv(data_file, check.names = FALSE)
  yobs <- calcIsos(yobs)
  obstime <- yobs$X
  yobs <- yobs[, !names(yobs) %in% c("X", "deltaTAN", "deltaNO3",
                                     "deltaN2O", "pH"), drop = FALSE]
  yobs <- calcDeltas(yobs)

  print(two_part_figure_with_obs(ysim, yobs, obstime))
  print(residuals_figures(ysim, yobs, obstime))

  sse <- as.numeric(ssqOdeModel(NULL, NANNOmodel, obstime,
                                yobs[, c("TAN", "NO3", "N2O",
                                         "isoTAN", "isoNO3", "isoN2O")]))
  message(sprintf("NANNO: SSE = %g", sse))
  invisible(list(ysim = ysim, yobs = yobs, obstime = obstime, SSE = sse))
}
