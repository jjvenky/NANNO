#' NANNO Model Function
#'
#' This function is the NANNO model. It is a forward-running model that can be coupled with a script to fit field data.
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
    eqmRatio <- (10^-9.25/10^-p["pH"])/(1 + 10^-9.25/10^-p["pH"])
    rnit1 <- p["knit1"] * x["NH4"]
    rnit2 <- p["knit2"] * x["NO2"]
    ramup <- p["kamup"] * x["NH4"]
    rniup <- p["kniup"] * x["NO3"]
    rdenit <- p["kdenit"] * x["NO3"]
    rge <- p["kge"] * x["NH3"]
    dNH3 <- (-p["kNH3NH4"] * x["NH3"]) + (p["kNH4NH3"] * x["NH4"]) - rge
    dNH4 <- (p["kNH3NH4"] * x["NH3"]) - (p["kNH4NH3"] * x["NH4"]) - rnit1 - ramup
    dNO2 <- rnit1 - rnit2
    dNO3 <- rnit2 - rdenit - rniup
    dTAN <- dNH3 + dNH4
    dN2O <- rdenit
    disoNH3 <- (-p["kNH3NH4"] * x["NH3"]) * (p["aNH3NH4"] * x["isoNH3"]/x["NH3"]) +
      (p["kNH4NH3"] * x["NH4"]) * (p["aNH4NH3"] * x["isoNH4"]/x["NH4"]) -
      rge * (p["age"] * x["isoNH3"]/x["NH3"])
    disoNH4 <- (p["kNH3NH4"] * x["NH3"]) * (p["aNH3NH4"] * x["isoNH3"]/x["NH3"]) -
      (p["kNH4NH3"] * x["NH4"]) * (p["aNH4NH3"] * x["isoNH4"]/x["NH4"]) -
      rnit1 * (p["anit1"] * x["isoNH4"]/x["NH4"]) -
      ramup * (p["aamup"] * x["isoNH4"]/x["NH4"])
    disoNO2 <- rnit1 * (p["anit1"] * x["isoNH4"]/x["NH4"]) - rnit2 * (p["anit2"] * x["isoNO2"]/x["NO2"])
    disoNO3 <- rnit2 * (p["anit2"] * x["isoNO2"]/x["NO2"]) - rdenit * (p["adenit"] * x["isoNO3"]/x["NO3"]) - rniup * (p["aniup"] * x["isoNO3"]/x["NO3"])
    disoTAN <- disoNH3 + disoNH4
    disoN2O <- rdenit * (p["adenit"] * x["isoNO3"]/x["NO3"])
    list(c(dNH3, dNH4, dNO2, dNO3, dTAN, dN2O, disoNH3, disoNH4, disoNO2, disoNO3, disoTAN, disoN2O))
  }, parms = c(kNH3NH4 = 1/((10^-9.25/10^-8.5)/(1 + 10^-9.25/10^-8.5)),
               kNH4NH3 = 1,
               kge = 0.002,
               knit1 = 5e-04,
               knit2 = 0.002,
               kdenit = 1e-04,
               kamup = 0.001,
               kniup = 0,
               aNH3NH4 = 1/0.9775,
               aNH4NH3 = 0.9775,
               age = 0.995,
               anit1 = 0.99,
               anit2 = 0.99,
               adenit = 0.985,
               aamup = 0.996,
               aniup = 0.992,
               pH = 8.5),
  times = c(from = 0, to = 5000, by = 1),
  init = c(NH3 = 3.55, NH4 = 16.45, NO2 = 0.5, NO3 = 3, TAN = 20, N2O = 0.01,
           isoNH3 = 3.55 * deltaToRatio(-27), isoNH4 = 16.45 * deltaToRatio(18),
           isoNO2 = 0.5 * deltaToRatio(8), isoNO3 = 3 * deltaToRatio(10),
           isoTAN = 3.55 * deltaToRatio(-27) + 16.45 * deltaToRatio(18),
           isoN2O = 0.01 * deltaToRatio(-20)),
  solver = "lsoda")
}

# aNH3NH4=1/0.9775, aNH4NH3=0.9775 since thise cuts the alpha in half so the total fractionation is correct concentrations in mgN/L
