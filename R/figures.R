# Internal colour palette shared by all figures (scales::hue_pal()(5))
nanno_colours <- c(NO2 = "#F8766D", NO3 = "#A3A500", TAN = "#00BF7D",
                   N2O = "#00B0F6", DIN = "#E76BF3",
                   deltaNO2 = "#F8766D", deltaNO3 = "#A3A500",
                   deltaTAN = "#00BF7D", deltaN2O = "#00B0F6",
                   deltaDIN = "#E76BF3")

# Internal figure builder: concentrations on the top row, delta values below.
# vars = concentration column names; delta_vars = matching delta column names.
nanno_two_part <- function(melted_top, melted_bottom, top_labels, bottom_labels,
                           y_limits_top, y_limits_bottom, x_label,
                           obs_top = NULL, obs_bottom = NULL) {
  g.top <- ggplot2::ggplot(melted_top,
                           ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE)
  if (!is.null(obs_top)) {
    g.top <- g.top + ggplot2::geom_point(data = obs_top, size = 4, na.rm = TRUE)
  }
  g.top <- g.top +
    ggplot2::labs(x = "", y = "Concentration (mgN/L)", colour = "") +
    ggplot2::coord_cartesian(ylim = y_limits_top) +
    ggplot2::theme(legend.position = "none", legend.box = "horizontal") +
    ggplot2::scale_colour_manual(labels = top_labels, values = nanno_colours)

  g.bottom <- ggplot2::ggplot(melted_bottom,
                              ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE)
  if (!is.null(obs_bottom)) {
    g.bottom <- g.bottom + ggplot2::geom_point(data = obs_bottom, size = 4, na.rm = TRUE)
  }
  g.bottom <- g.bottom +
    ggplot2::labs(x = x_label, y = expression("\u3b4"^{15}*"N (\u2030 vs air)"),
                  colour = "") +
    ggplot2::coord_cartesian(ylim = y_limits_bottom) +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_colour_manual(labels = bottom_labels, values = nanno_colours)

  gridExtra::grid.arrange(g.top, g.bottom, heights = c(0.5, 0.5))
}

# Internal helper: reshape simulation columns to long format
# (columns: time, variable, value) using tidyr rather than the
# superseded reshape2::melt().
nanno_melt <- function(d, vars) {
  keep <- intersect(c("time", vars), names(d))
  tidyr::pivot_longer(d[, keep, drop = FALSE], cols = -time,
                      names_to = "variable", values_to = "value")
}

#' Two Part Figure Function
#'
#' Takes model output and plots three concentrations (NO3-, TAN, DIN) on one
#' row and the matching delta values on another.
#'
#' @param ysim The NANNO model simulation results.
#' @return A grid of two ggplot objects arranged with gridExtra.
#' @export
#' @examples
#' ysim <- calcDeltas(simecol::out(simecol::sim(NANNO_model())))
#' two_part_figure(ysim)
two_part_figure <- function(ysim) {
  conc <- c("NO3", "TAN", "DIN")
  del <- paste0("delta", conc)
  nanno_two_part(
    nanno_melt(ysim, conc), nanno_melt(ysim, del),
    top_labels = list(expression(NO[3]^"-"), expression(TAN), expression(DIN)),
    bottom_labels = list(expression(NO[3]^"-"), expression(TAN), expression(DIN)),
    y_limits_top = range(ysim$DIN, na.rm = TRUE),
    y_limits_bottom = range(ysim$deltaDIN, na.rm = TRUE),
    x_label = "Distance (m)")
}

#' Two Part Figure Function with All Stocks
#'
#' Takes model output and plots four concentrations (NO3-, TAN, N2O, DIN) on
#' one row and the matching delta values on another.
#'
#' @param ysim The NANNO model simulation results.
#' @return A grid of two ggplot objects arranged with gridExtra.
#' @export
#' @examples
#' ysim <- calcDeltas(simecol::out(simecol::sim(NANNO_model())))
#' two_part_figure_all(ysim)
two_part_figure_all <- function(ysim) {
  conc <- c("NO3", "TAN", "N2O", "DIN")
  del <- paste0("delta", conc)
  nanno_two_part(
    nanno_melt(ysim, conc), nanno_melt(ysim, del),
    top_labels = list(expression(NO[3]^"-"), expression(TAN),
                      expression(N[2] * O), expression(DIN)),
    bottom_labels = list(expression(NO[3]^"-"), expression(TAN),
                         expression(N[2] * O), expression(DIN)),
    y_limits_top = range(ysim$DIN, na.rm = TRUE),
    y_limits_bottom = range(ysim$deltaDIN, ysim$deltaN2O, na.rm = TRUE),
    x_label = "Distance (m)")
}

#' Two Part Figure with Observations Function
#'
#' Takes model output and observations and plots the concentrations on one
#' row and the delta values on another, with observations as points.
#'
#' @param ysim The NANNO model simulation results.
#' @param yobs The field observations (with \code{deltaTAN} and
#'   \code{deltaNO3} columns, as returned by \code{calcDeltas}).
#' @param obstime The field observation times (distances).
#' @return A grid of two ggplot objects arranged with gridExtra.
#' @export
#' @examples
#' ysim <- calcDeltas(simecol::out(simecol::sim(NANNO_model())))
#' yobs <- calcDeltas(read.csv(system.file("extdata", "NANNO_example-data.csv",
#'                                         package = "NANNO"), check.names = FALSE))
#' two_part_figure_with_obs(ysim, yobs, yobs$X)
two_part_figure_with_obs <- function(ysim, yobs, obstime) {
  conc <- c("NO3", "TAN", "DIN")
  del <- paste0("delta", conc)
  obs <- cbind(time = obstime, yobs)
  obs_top <- nanno_melt(obs, conc)
  obs_bottom <- nanno_melt(obs, del)
  nanno_two_part(
    nanno_melt(ysim, conc), nanno_melt(ysim, del),
    top_labels = list(expression(NO[3]^"-"), expression(TAN), expression(DIN)),
    bottom_labels = list(expression(NO[3]^"-"), expression(TAN), expression(DIN)),
    y_limits_top = c(0, max(ysim$DIN, na.rm = TRUE) + 1),
    y_limits_bottom = range(ysim$deltaDIN, na.rm = TRUE),
    x_label = "Distance (m)",
    obs_top = obs_top, obs_bottom = obs_bottom)
}
