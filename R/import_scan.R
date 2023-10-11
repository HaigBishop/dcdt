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
  # Get the read name
  read_name <- lines[1]

  # Check the parameters are valid
  if (length(lines) < 2) {
    stop("Read parameters are missing.")
  }
  # Get all the read parameters from line 2
  read_params <- unlist(strsplit(trimws(lines[2]), " "))
  # Check there are 8 parameters and items 2-8 are numerical
  if (length(read_params) != 8 ||
      !all(!is.na(sapply(read_params[2:8], as.numeric)))) {
    stop("Invalid read parameters.")
  }
  # Extract and convert parameters to appropriate types
  scan_type <- read_params[1]
  rotor_pos <- as.integer(read_params[2])
  temp <- as.numeric(read_params[3])
  speed <- as.integer(read_params[4])
  time <- as.integer(read_params[5])
  x <- as.numeric(read_params[6])
  wavelength <- as.integer(read_params[7])
  num_replicates <- as.integer(read_params[8])

  # Initialise a data frame to hold the data (radius, x, y)
  read_data <- data.frame(radius = as.numeric(0), x = as.numeric(0), y = as.numeric(0))
  # Loop through all lines after line 2
  for (i in 3:length(lines)) {
    # Split the line by white space
    data_line <- unlist(strsplit(trimws(lines[i]), "\\s+"))
    # Check there are 3 numerical values
    if (length(data_line) != 3 ||
        !all(!is.na(sapply(data_line, as.numeric)))) {
      stop("Invalid read data.")
    }
    # Add new row to the data frame
    new_row <- data.frame(radius = as.numeric(data_line[1]), x = as.numeric(data_line[2]), y = as.numeric(data_line[3]))
    read_data <- rbind(read_data, new_row)
  }

  # Create a list with scan information
  scan <- list(
    name = read_name,
    scan_type = scan_type,
    rotor_pos = rotor_pos,
    temp = temp,
    speed = speed,
    time = time,
    x = x,
    wavelength = wavelength,
    num_replicates = num_replicates,
    data = read_data
  )
  # Turn it into an S3 "Scan" object
  class(scan) <- "Scan"
  return(scan)
}


