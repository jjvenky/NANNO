#' NANNO Inputs Function
#'
#' This function takes inputs for lower and upper bounds from a csv file and converts them for the fitting routine
#' @param filename The base name of the bounds file to be used.
#' @keywords NANNO
#' @export
#' @examples
#' NANNO_input_converting(filename)

NANNO_input_converting <- function (filename) {
  input <- read.csv(paste(paste(filename, "bounds", sep = "-"), "csv", sep = "."))
  setNames(data.frame(t(input[,-1])), input[,1])
  # Easier to send back a data.frame instead of a list
  # So separate out the three rows in another place as per:
  # lower <- data.frame(tmp["lower", ], row.names = c())
  # upper <- data.frame(tmp["upper", ], row.names = c())
  # initial <- data.frame(tmp["initial", ], row.names = c())
}
