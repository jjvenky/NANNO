#' Ratio To Delta Function
#'
#' This function converts isotopic ratios into delta values for nitrogen.
#' @param ratio The 15N/14N isotopic ratios.
#' @keywords NANNO
#' @export
#' @examples
#' ratioToDelta(ratio)

ratioToDelta <- function(ratio) {
    (ratio/0.0036765 - 1) * 1000
}



#' Ratio To Delta Function
#'
#' This function converts delta values into isotopic ratios for nitrogen.
#' @param delta The delta15N values.
#' @keywords NANNO
#' @export
#' @examples
#' deltaToRatio(delta)

deltaToRatio <- function(delta) {
    (delta/1000 + 1) * 0.0036765
}
