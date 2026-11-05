#' Test the NANNO Model
#'
#' Runs the default \code{NANNO_model()}, converts the isotope pools to delta
#' values, and draws the demonstration figure.
#'
#' @return The two-part figure (invisibly).
#' @export
#' @examples
#' NANNO_test()
NANNO_test <- function() {
  message('Running basic NANNO model.')
  NANNO.out <- calcDeltas(simecol::out(simecol::sim(NANNO_model())))
  message('Creating basic plot of NANNO model results.')
  invisible(two_part_figure_all(NANNO.out))
}
