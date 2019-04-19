#' Two Part Figure Function
#'
#' This function takes model simulations and plots them with 3 concentrations (NO3-, TAN, DIN) on one row and isotopes on another.
#' Colours are from scales::hue_pal()(5) to use the same colour for all 5 potential variables
#' "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"
#' See https://github.com/hadley/scales/blob/master/R/pal-hue.r
#' @param ysim The NANNO model simulation results.
#' @keywords NANNO
#' @export
#' @examples
#' two_part_figure(ysim)

two_part_figure <- function(ysim) {
  # NANNO_colours <- scales::hue_pal()(5)
  NANNO_colours <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")
  NANNO_colours <- c("NO2" = NANNO_colours[1], "NO3" = NANNO_colours[2], "TAN" = NANNO_colours[3], "N2O" = NANNO_colours[4], "DIN" = NANNO_colours[5],
                     "deltaNO2" = NANNO_colours[1], "deltaNO3" = NANNO_colours[2], "deltaTAN" = NANNO_colours[3], "deltaN2O" = NANNO_colours[4], "deltaDIN" = NANNO_colours[5])
  g.top <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, NO3, TAN, DIN)), id.vars = "time"),
                           ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(x = "", y = "Concentration (mgN/L)", colour = "") +
    ggplot2::theme(legend.position = "none", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, 15)) +
    ggplot2::scale_colour_manual(labels = c(expression(NO[3]^"-"), expression(TAN), expression(DIN)),
                                 values = NANNO_colours)
  g.bottom <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, deltaNO3, deltaTAN, deltaDIN)), id.vars = "time"),
                              ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(x = "Distance (m)", y = expression("\u3b4"^{15}*"N (\u2030 vs air)"), colour = "") +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, 40)) +
    ggplot2::scale_colour_manual(labels = c(expression(NO[3]^"-"), expression(TAN), expression(DIN)),
                                   values = NANNO_colours)
  g.all <- gridExtra::grid.arrange(g.top, g.bottom, heights = c(0.5, 0.5))
  return(g.all)
}
##########

#' Two Part Figure Function with All Stocks
#'
#' This function takes model simulations and plots them with all 4 concentrations (NO3-, TAN, N2O, DIN) on one row and isotopes on another.
#' Colours are from scales::hue_pal()(5) to use the same colour for all 5 potential variables
#' "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"
#' See https://github.com/hadley/scales/blob/master/R/pal-hue.r
#' @param ysim The NANNO model simulation results.
#' @keywords NANNO
#' @export
#' @examples
#' two_part_figure(ysim)

two_part_figure_all <- function(ysim) {
  # NANNO_colours <- scales::hue_pal()(5)
  NANNO_colours <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")
  NANNO_colours <- c("NO2" = NANNO_colours[1], "NO3" = NANNO_colours[2], "TAN" = NANNO_colours[3], "N2O" = NANNO_colours[4], "DIN" = NANNO_colours[5],
                     "deltaNO2" = NANNO_colours[1], "deltaNO3" = NANNO_colours[2], "deltaTAN" = NANNO_colours[3], "deltaN2O" = NANNO_colours[4], "deltaDIN" = NANNO_colours[5])
  g.top <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, NO3, TAN, N2O, DIN)), id.vars = "time"),
                           ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(x = "", y = "Concentration (mgN/L)", colour = "") +
    ggplot2::theme(legend.position = "none", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, 25)) +
    ggplot2::scale_colour_manual(labels = c(expression(NO[3]^"-"), expression(TAN), expression(N[2] * O), expression(DIN)),
                                 values = NANNO_colours)
  g.bottom <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, deltaNO3, deltaTAN, deltaN2O, deltaDIN)), id.vars = "time"),
                              ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(x = "Distance (m)", y = expression("\u3b4"^{15}*"N (\u2030 vs air)"), colour = "") +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(-21, 40)) +
    ggplot2::scale_colour_manual(labels = c(expression(NO[3]^"-"), expression(TAN), expression(N[2] * O), expression(DIN)),
                                   values = NANNO_colours)
  g.all <- gridExtra::grid.arrange(g.top, g.bottom, heights = c(0.5, 0.5))
  return(g.all)
}
##########

#' Two Part Figure with Observations Function
#'
#' This function takes model simulations and observations and plots them with concentrations on one row and isotopes on another.
#' Colours are from scales::hue_pal()(5) to use the same colour for all 5 potential variables
#' "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"
#' See https://github.com/hadley/scales/blob/master/R/pal-hue.r
#' @param ysim The NANNO model simulation results.
#' @param yobs The field observations.
#' @param obstime Field observation times
#' @keywords NANNO
#' @export
#' @examples
#' two_part_figure_with_obs(ysim, yobs)

two_part_figure_with_obs <- function(ysim, yobs, obstime) {
  # NANNO_colours <- scales::hue_pal()(5)
  NANNO_colours <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")
  NANNO_colours <- c("NO2" = NANNO_colours[1], "NO3" = NANNO_colours[2], "TAN" = NANNO_colours[3], "N2O" = NANNO_colours[4], "DIN" = NANNO_colours[5],
                     "deltaNO2" = NANNO_colours[1], "deltaNO3" = NANNO_colours[2], "deltaTAN" = NANNO_colours[3], "deltaN2O" = NANNO_colours[4], "deltaDIN" = NANNO_colours[5])
  g.top <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, NO3, TAN, DIN)), id.vars = "time"),
                           ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1, na.rm = TRUE) +
    ggplot2::geom_point(data = reshape2::melt(subset(plyr::rename(cbind(obstime, yobs), c(obstime = "time")), select = c(time, NO3, TAN, DIN)), id.vars = "time"),
                        size = 4, na.rm = TRUE) +
    ggplot2::labs(x = "", y = "Concentration (mgN/L)", colour = "") +
    ggplot2::theme(legend.position = "none", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, max(ysim$DIN, rm.na = TRUE) + 1)) +
    ggplot2::scale_colour_manual(labels = c(expression(NO[3]^"-"), expression(TAN), expression(DIN)),
                                 values = NANNO_colours)
  g.bottom <- ggplot2::ggplot(reshape2::melt(subset(ysim, select = c(time, deltaNO3, deltaTAN, deltaDIN)), id.vars = "time"),
                              ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1, na.rm = TRUE) +
    ggplot2::geom_point(data = reshape2::melt(subset(plyr::rename(cbind(obstime, yobs), c(obstime = "time")), select = c(time, deltaNO3, deltaTAN, deltaDIN)), id.vars = "time"),
                        size = 4, na.rm = TRUE) +
    ggplot2::labs(x = "Distance (m)", y = expression("\u3b4"^{15}*"N (\u2030 vs air)"), colour = "") +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::scale_y_continuous(limits = c(0, 40)) +
    ggplot2::scale_colour_manual(labels = c(expression(NO[3]^"-"), expression(TAN), expression(DIN)),
                                 values = NANNO_colours)
  g.all <- gridExtra::grid.arrange(g.top, g.bottom, heights = c(0.5, 0.5))
  return(g.all)
}
###########
