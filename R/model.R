#' NANNO Model Function
#'
#' Creates the NANNO nitrogen isotope biogeochemical model as a simecol
#' \code{odeModel} object. It is a forward-running model that can be run
#' directly or coupled to field data with \code{NANNO_fit()}.
#' By default it contains initial conditions and parameters so that it can be
#' run as a demonstration; these values are adjustable via the usual simecol
#' interface.
#'
#' @return A simecol \code{odeModel} S4 object.
#' @export
#' @examples
#' NANNO_model()
NANNO_model <- function() {
  new("odeModel",
      main = function(time, init, parms) {
        with(as.list(c(init, parms)), {
          TAN_vals <- TAN_speciation(TAN, pH)
          isoTAN_vals <- TAN_isotope_speciation(TAN, isoTAN, pH)
          rnit1 <- knit1 * TAN_vals["NH4"]
          ramup <- kamup * TAN_vals["NH4"]
          rdenit <- kdenit * NO3
          rge <- kge * TAN_vals["NH3"]
          dTAN <- - rge - rnit1 - ramup
          dNO3 <- rnit1 - rdenit
          dN2O <- rdenit
          disoTAN <- - rge * (age * isoTAN_vals["isoNH3"] / TAN_vals["NH3"]) -
            rnit1 * (anit1 * isoTAN_vals["isoNH4"] / TAN_vals["NH4"]) -
            ramup * (aamup * isoTAN_vals["isoNH4"] / TAN_vals["NH4"])
          disoNO3 <- rnit1 * (anit1 * isoTAN_vals["isoNH4"] / TAN_vals["NH4"]) -
            rdenit * (adenit * isoNO3 / NO3)
          disoN2O <- rdenit * (adenit * isoNO3 / NO3)
          list(c(dTAN, dNO3, dN2O, disoTAN, disoNO3, disoN2O))
        })
      },
      parms = c(kge = 0.002,
                knit1 = 0.002,
                kdenit = 1e-04,
                kamup = 0.001,
                age = 0.995,
                anit1 = 0.99,
                adenit = 0.985,
                aamup = 0.996,
                pH = 8.5),
      times = c(from = 0, to = 5000, by = 1),
      init = c(TAN = 20, NO3 = 3, N2O = 0.01,
               isoTAN = 20 * deltaToRatio(10),
               isoNO3 = 3 * deltaToRatio(10),
               isoN2O = 0.01 * deltaToRatio(-20)),
      solver = "lsoda")
}
