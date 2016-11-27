#' Two Part Figure Function
#'
#' This function takes model simulations and plots them with 3 concentrations (NO3-, TAN, DIN) on one row and isotopes on another.
#' @param ysim The NANNO model simulation results.
#' @keywords NANNO
#' @export
#' @examples
#' two_part_figure(ysim)

two_part_figure <- function(ysim) {
  g.top <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, NO3, TAN, DIN)), id.vars = "time"),
                           ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(x = "", y = "Concentration (mgN/L)", colour = "") +
    ggplot2::theme(legend.position = "none", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, 15)) +
    ggplot2::scale_colour_discrete(labels = c(expression(NO[3]^"-"), expression(TAN), expression(DIN)))
  g.bottom <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, deltaNO3, deltaTAN, deltaDIN)), id.vars = "time"),
                              ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(x = "Distance (m)", y = expression("\u3b4"^{15}*"N (\u2030 vs air)"), colour = "") +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, 40)) +
    ggplot2::scale_colour_discrete(labels = c(expression(NO[3]^"-"), expression(TAN), expression(DIN)))
  g.all <- gridExtra::grid.arrange(g.top, g.bottom, heights = c(0.5, 0.5))
  return(g.all)
}
##########

#' Two Part Figure Function with All Stocks
#'
#' This function takes model simulations and plots them with all 4 concentrations (NO3-, TAN, N2O, DIN) on one row and isotopes on another.
#' @param ysim The NANNO model simulation results.
#' @keywords NANNO
#' @export
#' @examples
#' two_part_figure(ysim)

two_part_figure_all <- function(ysim) {
  g.top <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, NO3, TAN, N2O, DIN)), id.vars = "time"),
                           ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(x = "", y = "Concentration (mgN/L)", colour = "") +
    ggplot2::theme(legend.position = "none", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, 25)) +
    ggplot2::scale_colour_discrete(labels = c(expression(NO[3]^"-"), expression(TAN), expression(N[2] * O), expression(DIN)))
  g.bottom <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, deltaNO3, deltaTAN, deltaN2O, deltaDIN)), id.vars = "time"),
                              ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(x = "Distance (m)", y = expression("\u3b4"^{15}*"N (\u2030 vs air)"), colour = "") +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(-21, 40)) +
    ggplot2::scale_colour_discrete(labels = c(expression(NO[3]^"-"), expression(TAN), expression(N[2] * O), expression(DIN)))
  g.all <- gridExtra::grid.arrange(g.top, g.bottom, heights = c(0.5, 0.5))
  return(g.all)
}
##########

#' Two Part Figure with Observations Function
#'
#' This function takes model simulations and observations and plots them with concentrations on one row and isotopes on another.
#' @param ysim The NANNO model simulation results.
#' @param yobs The field observations.
#' @param obstime Field observation times
#' @keywords NANNO
#' @export
#' @examples
#' two_part_figure_with_obs(ysim, yobs)

two_part_figure_with_obs <- function(ysim, yobs, obstime) {
  g.top <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, NO3, TAN, DIN)), id.vars = "time"),
                           ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1, na.rm = TRUE) +
    ggplot2::geom_point(data = reshape2::melt(subset(plyr::rename(cbind(obstime, yobs), c(obstime = "time")), select = c(time, NO3, TAN, DIN)), id.vars = "time"),
                        size = 4, na.rm = TRUE) +
    ggplot2::labs(x = "", y = "Concentration (mgN/L)", colour = "") +
    ggplot2::theme(legend.position = "none", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, max(ysim$DIN, rm.na = TRUE) + 1)) +
    ggplot2::scale_colour_discrete(labels = c(expression(NO[3]^"-"), expression(TAN), expression(DIN)))
  g.bottom <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, deltaNO3, deltaTAN, deltaDIN)), id.vars = "time"),
                              ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1, na.rm = TRUE) +
    ggplot2::geom_point(data = reshape2::melt(subset(plyr::rename(cbind(obstime, yobs), c(obstime = "time")), select = c(time, deltaNO3, deltaTAN, deltaDIN)), id.vars = "time"),
                        size = 4, na.rm = TRUE) +
    ggplot2::labs(x = "Distance (m)", y = expression("\u3b4"^{15}*"N (\u2030 vs air)"), colour = "") +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, 40)) + ggplot2::scale_colour_discrete(labels = c(expression(NO[3]^"-"), expression(TAN), expression(DIN)))
  g.all <- gridExtra::grid.arrange(g.top, g.bottom, heights = c(0.5, 0.5))
  return(g.all)
}
###########
