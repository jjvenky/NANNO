#' NANNO Fitting Function
#'
#' This function takes field data and iteratively fits
#' @param filename The base name of the files to be used in NANNO model fitting.
#' @keywords NANNO
#' @export
#' @examples
#' NANNO_fit(filename)

# Inputs for: 1) field data file, 2) error on measurements, 3) parameters to be fit, 4) bounds for parameters

# Structure:
# 1. Load data
# 2. Set whichpar, parms, init
# 3. Fit the model
# 4. Rerun model to get data for plotting
# 5. Convert data and plot
# 6. Calculate fit statistics
# 7. Write csv and pdf outputs


NANNO_fit <- function(filename) {
  # Runtime
  myRuntime <- format(Sys.time(), "%Y%m%dT%H%M%S")

  # Create copy of the model
  tm1 <- NANNO_model()

  # Load field data
  yobs <- read.csv(paste(paste(filename, "data", sep = "-"), "csv", sep = "."))

  # Load parameters to be fit and bounds for parameters
  tmp <- NANNO_input_converting(filename)
  lower <- data.frame(tmp["lower", ], row.names = c())
  upper <- data.frame(tmp["upper", ], row.names = c())
  initial <- data.frame(tmp["initial", ], row.names = c())
  rm(tmp)

  # Prepare data
  yobs <- calcIsos(yobs)
  obstime <- yobs$X
  obspH <- mean(yobs$pH, na.rm = TRUE)
  fNH3init <- (10^-9.25/10^-obspH)/(1+10^-9.25/10^-obspH)
  fNH4init <- 1 - fNH3init
  initdeltaTAN <- yobs[1, "deltaTAN"]
  yobs <- subset(yobs, select = -c(X, deltaNH3, deltaNH4, deltaNO2, deltaNO3, deltaTAN, deltaN2O, pH))

  # Set times
  times(tm1) <- obstime

  # Add initial values to the parameter vector
  # Assign the init() values from the field data
  # Assume only TAN and NO3 field data for now
  # Add conditions for NA values
  init(tm1) <-  c(NH3=fNH3init * yobs[1, "TAN"],
                  NH4=fNH4init * yobs[1, "TAN"],
                  NO2=0.5*yobs[1, "NO3"],
                  NO3=yobs[1, "NO3"],
                  TAN=yobs[1, "TAN"],
                  N2O=0.01,
                  isoNH3=fNH3init*yobs[1, "TAN"]*deltaToRatio(ratioToDelta(yobs[1, "isoTAN"]/yobs[1, "TAN"]) + fNH4init * yobs[1, "TAN"]),
                  isoNH4=fNH4init*yobs[1, "TAN"]*deltaToRatio(ratioToDelta(yobs[1, "isoTAN"]/yobs[1, "TAN"]) + fNH3init * yobs[1, "TAN"]),
                  isoNO2=0.5*yobs[1, "isoNO3"],
                  isoNO3=yobs[1, "isoNO3"],
                  isoTAN=yobs[1, "isoTAN"],
                  isoN2O=0.01*deltaToRatio(-20))
  parms(tm1) <- c(parms(tm1), init(tm1)) # Do the init() values need to be altered any other way?

  # Define an intifunc that copies these parameters back to init
  initfunc(tm1) <- function(obj) {
    init(obj) <- parms(obj)[c("NH3", "NH4", "NO2", "NO3", "TAN", "N2O", "isoNH3", "isoNH4", "isoNO2", "isoNO3", "isoTAN", "isoN2O")] # Note!  Order is important!
    obj
  }

  # Set *external* time step to same as in observations,
  # and use efficient algorithm with automatic *internal* time steps
  times(tm1) <- obstime
  solver(tm1) <- "lsoda"

  # Set whichpar to identify which parameters are to be adjusted
  # Set lower and upper bounds for those parameters
  # Currently only adjusting the TAN and NO3 init values since that is all we have measured
  # but will have to do this for all parameters to improve fit with real data
  # Consider do it using the values from init()
  whichpar  <- c("kge", "knit1", "knit2", "kdenit", "kamup", "anit1", "anit2", "adenit", "aamup", "NO3", "isoNO3", "TAN", "isoTAN")
  parms(tm1)[whichpar] <- c(kge = 0.002, knit1=0.001, knit2=0.005, kdenit=0.0001, kamup=0.001,
                            anit1 = 0.99, anit2 = 0.99, adenit = 0.985, aamup = 0.996,
                            NO3=yobs[1, "NO3"], isoNO3=yobs[1, "isoNO3"], TAN=yobs[1, "TAN"], isoTAN=yobs[1, "isoTAN"])
  lower <- c(kge = 0.0002, knit1=0.0001, knit2=0.0005, kdenit=0.00001, kamup=0.0001,
             anit1 = 0.975, anit2 = 0.975, adenit = 0.970, aamup = 0.980,
             NO3=0.99*yobs[1, "NO3"], isoNO3=0.9995*yobs[1, "isoNO3"], TAN=0.99*yobs[1, "TAN"], isoTAN=0.9995*yobs[1, "isoTAN"])
  upper <- c(kge = 0.02, knit1=0.02, knit2=0.04, kdenit=0.01, kamup=0.01,
             anit1 = 1, anit2 = 1, adenit = 1, aamup = 1,
             NO3=1.01*yobs[1, "NO3"], isoNO3=1.0005*yobs[1, "isoNO3"], TAN=1.01*yobs[1, "TAN"], isoTAN=1.0005*yobs[1, "isoTAN"])

  # Fit the data
  res <- fitOdeModel(tm1, whichpar = whichpar, obstime, yobs,
                     debuglevel=0, fn = ssqOdeModel,
                     method = "Nelder-Mead", lower = lower, upper = upper,
                     control = list(trace = TRUE),
                     atol=1e-9, rtol=1e-9)

  # Assign fitted parameters to scenario tm1
  parms(tm1)[whichpar] <- res$par

  # Set small external time step for good graphics and simulate again
  times(tm1) <- c(from=0, to=max(obstime), by=1)
  ysim1 <- out(sim(tm1))

  # Compare results
  ysim1 <- calcDeltas(ysim1)
  yobs <- calcDeltas(yobs)
  two_part_figure_with_obs(ysim1, yobs, obstime)

  print(res$par)
}




