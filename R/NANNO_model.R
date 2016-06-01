#' NANNO Model Function
#'
#' This function is the NANNO model. It is a forward-running model that can be coupled with a script to fit field data.
#' By default it contains initial conditions for the state values and parameters so that it can be run as a demonstration.
#' These values are adjustable and fitable with NANNO_fit() and the regular simeol interface.
#' @keywords NANNO
#' @export
#' @examples
#' NANNO_model()
#'

# The NANNO model as a function
NANNO_model <- function() {
  new("odeModel", main = function(time, init, parms) {
    x <- init
    p <- parms
    TAN_vals <- TAN_speciation(x["TAN"], p["pH"])
    isoTAN_vals <- TAN_isotope_speciation(x["TAN"], x["isoTAN"], p["pH"])
    eqmRatio <- (10^-9.25/10^-p["pH"])/(1 + 10^-9.25/10^-p["pH"])
    rnit1 <- p["knit1"] * TAN_vals["NH4"]
    rnit2 <- p["knit2"] * x["NO2"]
    ramup <- p["kamup"] * TAN_vals["NH4"]
    rniup <- p["kniup"] * x["NO3"]
    rdenit <- p["kdenit"] * x["NO3"]
    rge <- p["kge"] * TAN_vals["NH3"]
    dTAN <- - rge - rnit1 - ramup
    dNO2 <- rnit1 - rnit2
    dNO3 <- rnit2 - rdenit - rniup
    dN2O <- rdenit
    disoTAN <- - rge * (p["age"] * isoTAN_vals["isoNH3"]/TAN_vals["NH3"] ) -
      rnit1 * (p["anit1"] * isoTAN_vals["isoNH4"]/TAN_vals["NH4"]) -
      ramup * (p["aamup"] * isoTAN_vals["isoNH4"]/TAN_vals["NH4"])
    disoNO2 <- rnit1 * (p["anit1"] * isoTAN_vals["isoNH4"]/TAN_vals["NH4"]) -
      rnit2 * (p["anit2"] * x["isoNO2"]/x["NO2"])
    disoNO3 <- rnit2 * (p["anit2"] * x["isoNO2"]/x["NO2"]) -
      rdenit * (p["adenit"] * x["isoNO3"]/x["NO3"]) -
      rniup * (p["aniup"] * x["isoNO3"]/x["NO3"])
    disoN2O <- rdenit * (p["adenit"] * x["isoNO3"]/x["NO3"])
    list(c(dTAN, dNO2, dNO3, dN2O, disoTAN, disoNO2, disoNO3, disoN2O))
  }, parms = c(kge = 0.002,
               knit1 = 5e-04,
               knit2 = 0.002,
               kdenit = 1e-04,
               kamup = 0.001,
               kniup = 0,
               age = 0.995,
               anit1 = 0.99,
               anit2 = 0.99,
               adenit = 0.985,
               aamup = 0.996,
               aniup = 0.992,
               pH = 8.5),
  times = c(from = 0, to = 5000, by = 1),
  init = c(TAN = 20, NO2 = 0.5, NO3 = 3, N2O = 0.01,
           isoTAN = 20 * deltaToRatio(10),
           isoNO2 = 0.5 * deltaToRatio(8),
           isoNO3 = 3 * deltaToRatio(10),
           isoN2O = 0.01 * deltaToRatio(-20)),
  solver = "lsoda")
}
