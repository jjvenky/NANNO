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
  runtime <- format(Sys.time(), "%Y%m%dT%H%M%S")

  cat(c('Starting model',
        paste(filename, runtime, sep = "-"),
        '\n'))

  # Create copy of the model
  tm1 <- NANNO_model()

  # Load field data
  yobs <- read.csv(paste(paste(filename, "data", sep = "-"), "csv", sep = "."))

  # Load parameters to be fit and bounds for parameters
  tmp <- NANNO_input_converting(filename)

  # If some initial values are blank use the mean of the lower and upper
  # Can change this to random select a value if needed
  for(i in 1:ncol(tmp)){
    tmp[is.na(tmp[,i]), i] <- mean(tmp[,i], na.rm = TRUE)
  }
  # Need lower, upper, and initial as doubles with names
  lower <- structure(c(as.numeric(tmp["lower", ])), .Names = names(tmp["lower", ]))
  upper <- structure(c(as.numeric(tmp["upper", ])), .Names = names(tmp["upper", ]))
  initial <- structure(c(as.numeric(tmp["initial", ])), .Names = names(tmp["initial", ]))
  rm(tmp)

  # Prepare data
  yobs <- calcIsos(yobs)
  obstime <- yobs$X
  obspH <- mean(yobs$pH, na.rm = TRUE)
  fNH3init <- (10^-9.25/10^-obspH)/(1+10^-9.25/10^-obspH)
  fNH4init <- 1 - fNH3init
  initdeltaTAN <- yobs[1, "deltaTAN"]
  yobs <- subset(yobs, select = -c(X, deltaTAN, deltaNO2, deltaNO3, deltaN2O, pH))

  # Set times
  times(tm1) <- obstime

  # Add initial values to the parameter vector
  # Assign the init() values from the field data
  # Assume only TAN and NO3 field data for now
  # Add conditions for NA values
  init(tm1) <-  c(TAN=yobs[1, "TAN"],
                  NO2=0.5*yobs[1, "NO3"],
                  NO3=yobs[1, "NO3"],
                  N2O=0.01,
                  isoTAN=yobs[1, "isoTAN"],
                  isoNO2=0.5*yobs[1, "isoNO3"],
                  isoNO3=yobs[1, "isoNO3"],
                  isoN2O=0.01*deltaToRatio(-20))
  parms(tm1) <- c(parms(tm1), init(tm1)) # Do the init() values need to be altered any other way?

  # Define an intifunc that copies these parameters back to init
  initfunc(tm1) <- function(obj) {
    init(obj) <- parms(obj)[c("TAN", "NO2", "NO3", "N2O", "isoTAN", "isoNO2", "isoNO3", "isoN2O")] # Note!  Order is important!
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
  parms(tm1)[whichpar] <- c(initial,
                            NO3=yobs[1, "NO3"], isoNO3=yobs[1, "isoNO3"], TAN=yobs[1, "TAN"], isoTAN=yobs[1, "isoTAN"])
  lower <- c(lower,
             NO3=0.999*yobs[1, "NO3"], isoNO3=0.9995*yobs[1, "isoNO3"], TAN=0.999*yobs[1, "TAN"], isoTAN=0.9995*yobs[1, "isoTAN"])
  upper <- c(upper,
             NO3=1.001*yobs[1, "NO3"], isoNO3=1.0005*yobs[1, "isoNO3"], TAN=1.001*yobs[1, "TAN"], isoTAN=1.0005*yobs[1, "isoTAN"])

  # Fit the data
  cat(c('Running model',
        paste(filename, runtime, sep = "-"),
        '\n'))
  res <- fitOdeModel(tm1, whichpar = whichpar, obstime, yobs,
                     debuglevel=1, fn = ssqOdeModel,
                     method = "newuoa", lower = lower, upper = upper,
                     control = list(trace = TRUE),
                     atol=1e-9, rtol=1e-9,
                     scale.par = 1/upper)

  # Assign fitted parameters to scenario tm1
  parms(tm1)[whichpar] <- res$par

  # Set small external time step for good graphics and simulate again
  times(tm1) <- c(from=0, to=max(obstime), by=1)
  ysim1 <- out(sim(tm1))

  # Compare results
  ysim1 <- calcDeltas(ysim1)
  yobs <- calcDeltas(yobs)
  two_part_figure_with_obs(ysim1, yobs, obstime)

  cat(c('NANNO results',
        paste(filename, runtime, sep = "-"),
        '\n'))

  print(res$par)

  fit_stats <- NANNO_fit_stats(ysim1, yobs, obstime)
  fit_params <- NANNO_fit_params(tm1, whichpar)
  fit_masses <- NANNO_calc_masses(tm1, ysim1, obspH)
  write.csv(fit_stats, file = paste(paste(filename, runtime, "fit_stats", sep = "-"), "csv", sep = "."))
  write.csv(fit_params, file = paste(paste(filename, runtime, "fit_params", sep = "-"), "csv", sep = "."), row.names = TRUE)
  write.csv(fit_masses, file = paste(paste(filename, runtime, "fit_masses", sep = "-"), "csv", sep = "."), row.names = FALSE)

  return(tm1)
}




