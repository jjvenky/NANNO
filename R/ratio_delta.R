#' Ratio To Delta Function
#'
#' Converts isotopic ratios into delta values for nitrogen.
#'
#' @param ratio The 15N/14N isotopic ratios.
#' @return The corresponding delta15N values in permil.
#' @export
#' @examples
#' ratioToDelta(0.0036765 * 1.005)
ratioToDelta <- function(ratio) {
  (ratio / nanno_constants$r_std - 1) * 1000
}

#' Delta To Ratio Function
#'
#' Converts delta values into isotopic ratios for nitrogen.
#'
#' @param delta The delta15N values in permil.
#' @return The corresponding 15N/14N isotopic ratios.
#' @export
#' @examples
#' deltaToRatio(5)
deltaToRatio <- function(delta) {
  (delta / 1000 + 1) * nanno_constants$r_std
}
