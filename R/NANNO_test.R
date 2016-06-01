#' Test the NANNO Function
#'
#' This function takes model results from the default NANNO_model() and creates a default figure
#' @keywords NANNO
#' @export
#' @examples
#' NANNO_test()

NANNO_test <- function() {
  cat('Running basic NANNO model.\n')
  NANNO.out <- simecol::out(simecol::sim(NANNO_model()))
  NANNO.out <- calcDeltas(NANNO.out)
  NANNO.out.melt <- reshape2::melt(subset(NANNO.out, select = -c(isoTAN, isoNO2, isoNO3, isoN2O)), id.vars = "time")
  cat('Creating basic plot of NANNO model results.')
  ggplot2::ggplot(NANNO.out.melt, ggplot2::aes(x = time, y = value, colour = variable)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::facet_wrap(~variable, scales = "free", ncol = 5) +
    ggplot2::guides(colour = FALSE)
}
