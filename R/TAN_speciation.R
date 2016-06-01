#' TAN Speciation Function
#'
#' This function takes TAN concentration and pH and calculates/returns the concentration of NH3 and NH4+ as a vector.
#' @param TAN The TAN concentration.
#' @param pH The pH.
#' @keywords NANNO
#' @export
#' @examples
#' TAN_speciation(TAN, pH)

TAN_speciation <- function (TAN, pH) {
  fNH3 <- (10^-9.25/10^-pH)/(1+10^-9.25/10^-pH)
  fNH4 <- 1 - fNH3
  tmp <- c(NH3 = as.numeric(TAN * fNH3),
           NH4 = as.numeric(TAN * fNH4)
  )
  return(tmp)
}


#' TAN Isotope Speciation Function
#'
#' This function takes TAN concentration and pH and calculates/returns the iso values of NH3 and NH4+ as a vector.
#' @param TAN The TAN concentration.
#' @param isoTAN The isoTAN value (TAN isotopic ratio * TAN concentration)
#' @param pH The pH.
#' @keywords NANNO
#' @export
#' @examples
#' TAN_isotope_speciation(TAN, isoTAN, pH)

TAN_isotope_speciation <- function (TAN, isoTAN, pH) {
  fNH3 <- (10^-9.25/10^-pH)/(1+10^-9.25/10^-pH)
  fNH4 <- 1 - fNH3
  NH3 <- TAN * fNH3
  NH4 <- TAN * fNH4
  aeq <- 0.955
  # 1. Divide up the 45 permil (0.955) between NH3 and NH4 with the fNH3 and fNH4 values
  # 2. Convert that back to a regular alpha by subtracting that by 1
  # 3. Multiply this alpha (TAN-NH3 or TAN-NH4 alpha value) by isoTAN/TAN (to covnert that to a ratio)
  # 4. Multiply by the mass (NH3 or NH4) to get the iso value to return
  tmp <-  c(isoNH3 = as.numeric(isoTAN * (1 - ((1 - aeq) * fNH4)) / TAN * NH3),
            isoNH4 = as.numeric(isoTAN * (1 - ((1 - aeq) * fNH3))^-1 / TAN * NH4)
  )
  return(tmp)
}
