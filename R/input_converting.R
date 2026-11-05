#' NANNO Inputs Function
#'
#' Reads the lower/upper/initial bounds for the fit parameters from a CSV file
#' with columns \code{variable}, \code{lower}, \code{upper}, and
#' \code{initial}, and returns them as a transposed data frame with one row
#' per bound type. Missing values are left in place for the caller to fill.
#'
#' @param filename The base name of the bounds file (without extension) or the
#'   full path to a \code{*-bounds.csv} file.
#' @return A data frame with rows \code{lower}, \code{upper}, and
#'   \code{initial}, and one column per variable.
#' @export
#' @examples
#' NANNO_input_converting(system.file("extdata", "NANNO_example-bounds.csv",
#'                                    package = "NANNO"))
NANNO_input_converting <- function(filename) {
  bounds_file <- if (grepl("\\.csv$", filename)) {
    filename
  } else {
    paste(paste(filename, "bounds", sep = "-"), "csv", sep = ".")
  }
  input <- read.csv(bounds_file, check.names = FALSE)
  needed <- c("variable", "lower", "upper", "initial")
  missing_cols <- setdiff(needed, names(input))
  if (length(missing_cols) > 0) {
    stop("Bounds file is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  setNames(data.frame(t(input[, -1], deparse.level = 0),
                      stringsAsFactors = FALSE),
           input[, 1])
}
