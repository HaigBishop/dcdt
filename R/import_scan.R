#' Import raw data from an AUC scan into R
#'
#' This function reads a "scan file" which contains the data from a particular
#' scan in an AUC run.
#' This function is employed by dcdt::import_run to import all scans in the run.
#' The data in the scan files is the name (on line 1), the scan parameters
#' (on line 2), and the data itself (on all following lines).
#' This data is returned in the form of a S3 "Scan" object.
#'
#' @param scan_path The location of the file containing the AUC scan data.
#'
#' @return The AUC data from the scan in the form of an S3 "Scan" object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the scan in scan_file.RA2
#' auc_scan_1 <- dcdt::import_scan("path/to/your/scan_file.RA2")
#' }
#'
import_scan <- function(scan_path) {
  # Check the file exists
  if (!file.exists(scan_path)) {
    stop("File does not exist.")
  }

  # Read the file
  lines <- readLines(scan_path)

  # Check the name is non-empty
  if (length(lines) < 1 || nchar(lines[1]) == 0) {
    stop("Read name is empty or missing.")
  }

  # Check the parameters are valid
  if (length(lines) < 2) {
    stop("Read parameters are missing.")
  }

  # Get all the read parameters from line 2
  read_params <- strsplit(lines[2], " ")[[1]]
  # Check there are 8 parameters
  if (length(read_params) != 8) {
    stop("Invalid number of read parameters.")
  }

  # Initialise a list to hold vectors of each line c(radius, x, y)
  reads_list <- list()
  # Loop through all lines after line 2
  for (i in 3:length(lines)) {
    # Split the line by white space and make floats
    read <- as.numeric(strsplit(trimws(lines[i]), "\\s+")[[1]])
    # Check there are 3 numerical values
    if (length(read) != 3 || !all(!is.na(read))) {
      stop("Invalid read data.")
    }
    # Add this "row" to the list
    reads_list <- c(reads_list, list(read))
  }
  reads_df <- as.data.frame(do.call(rbind, reads_list))
  colnames(reads_df) <- c("radius", "x", "y")

  # Create a list with scan information (converting to proper type)
  scan <- list(
    name = lines[1], # The first line of file has name
    scan_type = read_params[1],
    rotor_pos = as.integer(read_params[2]),
    temp = as.numeric(read_params[3]),
    speed = as.integer(read_params[4]),
    time = as.integer(read_params[5]),
    x = as.numeric(read_params[6]),
    wavelength = as.integer(read_params[7]),
    num_replicates = as.integer(read_params[8]),
    data = reads_df # The actual data
  )
  # Turn it into an S3 "Scan" object
  class(scan) <- "Scan"
  return(scan)
}


