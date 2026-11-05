#' NANNO Fitting Function
#'
#' Takes field data plus fit-parameter bounds, fits the NANNO model with
#' \code{simecol::fitOdeModel()}, and writes timestamped CSV statistics and
#' PDF figures next to the input files.
#'
#' @param filename The base name of the input files: reads
#'   \code{<filename>-data.csv} for field observations and
#'   \code{<filename>-bounds.csv} for parameter bounds. Alternatively, pass
#'   the full path of the data file; the bounds file is then expected beside
#'   it with the same stem.
#' @param output_dir Directory in which output files are written.
#'   Default is the directory containing the data file.
#' @param init_bound_tol Relative tolerance for the initial-condition fit
#'   bounds (e.g. 0.001 fits the initial concentrations within 0.1 percent).
#' @param delta_bound_tol Relative tolerance for the initial isotope-ratio
#'   fit bounds.
#' @return A list with the fitted model object (\code{model}), simulated and
#'   observed data (\code{ysim}, \code{yobs}), fit statistics
#'   (\code{fit_stats}), fitted parameters (\code{fit_params}), and
#'   integrated process masses (\code{fit_masses}).
#' @seealso \code{\link{recreate_NANNO_fit}}
#' @export
#' @examples
#' \dontrun{
#' result <- NANNO_fit("NANNO_example")
#' }
NANNO_fit <- function(filename, output_dir = NULL, init_bound_tol = 0.001,
                      delta_bound_tol = 0.0005) {
  runtime <- format(Sys.time(), "%Y%m%dT%H%M%S")
  outfile <- function(tag, ext) {
    dir <- if (is.null(output_dir)) dirname(filename) else output_dir
    base <- tools::file_path_sans_ext(basename(filename))
    file.path(dir, paste(base, runtime, tag, ext, sep = "-"))
  }
  message(sprintf("NANNO: starting model %s (%s)", filename, runtime))

  tm1 <- NANNO_model()

  # Locate and load the field data
  data_file <- if (grepl("\\.csv$", filename)) {
    filename
  } else {
    paste(paste(filename, "data", sep = "-"), "csv", sep = ".")
  }
  if (!file.exists(data_file)) {
    stop("Field data file not found: ", data_file, call. = FALSE)
  }
  yobs <- read.csv(data_file, check.names = FALSE)

  # Load parameters to be fit and their bounds
  bounds_file <- if (grepl("\\.csv$", filename)) {
    file.path(dirname(filename),
              paste(paste(tools::file_path_sans_ext(basename(filename)),
                          "bounds", sep = "-"), "csv", sep = "."))
  } else {
    paste(paste(filename, "bounds", sep = "-"), "csv", sep = ".")
  }
  if (!file.exists(bounds_file)) {
    stop("Bounds file not found: ", bounds_file, call. = FALSE)
  }
  tmp <- NANNO_input_converting(bounds_file)

  # Fill missing bound values with the mean of the existing values in that
  # same row (lower, upper, or initial), not across rows.
  for (rw in rownames(tmp)) {
    vals <- as.numeric(tmp[rw, ])
    if (any(is.na(vals))) {
      if (all(is.na(vals))) {
        stop("Bounds row '", rw, "' has no values to fill from.", call. = FALSE)
      }
      tmp[rw, is.na(tmp[rw, ])] <- mean(vals, na.rm = TRUE)
    }
  }
  lower <- setNames(as.numeric(tmp["lower", ]), colnames(tmp))
  upper <- setNames(as.numeric(tmp["upper", ]), colnames(tmp))
  initial <- setNames(as.numeric(tmp["initial", ]), colnames(tmp))
  rm(tmp)

  # Prepare data
  yobs <- calcIsos(yobs)
  obstime <- yobs$X
  obspH <- mean(yobs$pH, na.rm = TRUE)
  drop_cols <- c("X", "deltaTAN", "deltaNO3", "deltaN2O", "pH")
  yobs <- yobs[, !names(yobs) %in% drop_cols, drop = FALSE]

  # Set external time step to the observation times and use an efficient
  # solver with automatic internal time steps
  times(tm1) <- obstime
  solver(tm1) <- "lsoda"

  # Assign initial values from the field data
  # Assume only TAN and NO3 field data for now
  init(tm1) <- c(TAN = yobs[1, "TAN"],
                 NO3 = yobs[1, "NO3"],
                 N2O = 0.01,
                 isoTAN = yobs[1, "isoTAN"],
                 isoNO3 = yobs[1, "isoNO3"],
                 isoN2O = 0.01 * deltaToRatio(-20))
  # Parameters carry the initial conditions so the fitter can adjust them
  parms(tm1) <- c(parms(tm1), init(tm1))
  # Copy the (possibly refit) initial conditions back into the state vector
  initfunc(tm1) <- function(obj) {
    state_names <- c("TAN", "NO3", "N2O", "isoTAN", "isoNO3", "isoN2O")
    p <- parms(obj)
    if (!all(state_names %in% names(p))) {
      stop("Initial-condition parameters are missing from the model parms.",
           call. = FALSE)
    }
    init(obj) <- p[state_names]
    obj
  }

  # Parameters to be adjusted: rates and alpha factors, plus the initial
  # TAN and NO3 conditions
  whichpar <- c("kge", "knit1", "kdenit", "kamup", "anit1", "adenit", "aamup",
                "NO3", "isoNO3", "TAN", "isoTAN")
  parms(tm1)[whichpar] <- c(initial,
                            NO3 = yobs[1, "NO3"], isoNO3 = yobs[1, "isoNO3"],
                            TAN = yobs[1, "TAN"], isoTAN = yobs[1, "isoTAN"])
  lower <- c(lower,
             NO3 = (1 - init_bound_tol) * yobs[1, "NO3"],
             isoNO3 = (1 - delta_bound_tol) * yobs[1, "isoNO3"],
             TAN = (1 - init_bound_tol) * yobs[1, "TAN"],
             isoTAN = (1 - delta_bound_tol) * yobs[1, "isoTAN"])
  upper <- c(upper,
             NO3 = (1 + init_bound_tol) * yobs[1, "NO3"],
             isoNO3 = (1 + delta_bound_tol) * yobs[1, "isoNO3"],
             TAN = (1 + init_bound_tol) * yobs[1, "TAN"],
             isoTAN = (1 + delta_bound_tol) * yobs[1, "isoTAN"])

  # Fit the data
  message(sprintf("NANNO: running model %s (%s)", filename, runtime))
  res <- fitOdeModel(tm1, whichpar = whichpar, obstime, yobs,
                     debuglevel = 0, fn = ssqOdeModel,
                     method = "bobyqa", lower = lower, upper = upper,
                     control = list(iprint = 2),
                     atol = 1e-6, rtol = 1e-6)

  # Assign fitted parameters to the scenario
  parms(tm1)[whichpar] <- res$par

  # Rerun with a small external time step for good graphics
  times(tm1) <- c(from = 0, to = max(obstime), by = 1)
  ysim1 <- calcDeltas(out(sim(tm1)))
  yobs <- calcDeltas(yobs)

  message(sprintf("NANNO: results %s (%s)", filename, runtime))
  g <- two_part_figure_with_obs(ysim1, yobs, obstime)
  cairo_pdf(filename = outfile("fit", "pdf"), width = 5, height = 7)
  on.exit(if (grDevices::dev.cur() > 1) grDevices::dev.off(), add = TRUE)
  grid::grid.draw(g)
  grDevices::dev.off()

  fit_stats <- NANNO_fit_stats(ysim1, yobs, obstime)
  fit_params <- NANNO_fit_params(tm1, whichpar)
  fit_masses <- NANNO_calc_masses(tm1, ysim1, obspH)
  write.csv(fit_stats, file = outfile("fit_stats", "csv"), row.names = TRUE)
  write.csv(fit_params, file = outfile("fit_params", "csv"), row.names = TRUE)
  write.csv(fit_masses, file = outfile("fit_masses", "csv"), row.names = FALSE)

  sse <- as.numeric(ssqOdeModel(NULL, tm1, obstime,
                                yobs[, c("TAN", "NO3", "N2O",
                                         "isoTAN", "isoNO3", "isoN2O")]))
  message(sprintf("NANNO: SSE %s (%s) = %g", filename, runtime, sse))

  g <- residuals_figures(ysim1, yobs, obstime)
  cairo_pdf(filename = outfile("resid", "pdf"), width = 7, height = 5)
  grid::grid.draw(g)
  grDevices::dev.off()

  invisible(list(model = tm1, ysim = ysim1, yobs = yobs, obstime = obstime,
                 fit_stats = fit_stats, fit_params = fit_params,
                 fit_masses = fit_masses, SSE = sse))
}
