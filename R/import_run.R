#' Import raw data from an AUC run into R
#'
#' This function reads a "list file" which contains a list of AUC scan files.
#' Each scan file is imported using dcdt::import_scan.
#' The data is returned in the form of a S4 "Run" object.
#'
#' @param list_path The path of the file containing the list of AUC read files.
#' @param dir The path to the directory which contains the read files.
#'
#' @return The AUC data from the run in the form of an S4 "Run" object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the run described by the scan files listed in list_file.RA2
#' auc_run_1 <- dcdt::import_run("path/to/your/list_file.RA2")
#'
#' # Load the run described by the scan files listed in list_file.RA2 and
#' # located in C:/Users/haggi/dcdt_data/
#' auc_run_2 <- dcdt::import_run("path/to/your/list_file.RA2",
#'   dir="C:/Users/haggi/dcdt_data/")
#' }
#'
import_run <- function(list_path, dir = NULL) {
  # Check list file exists
  if (!file.exists(list_path)) {
    stop("List file does not exist.")
  }

  # Read the list file and store the lines in a list
  lines <- readLines(list_path)

  # If the directory was unspecified, use the directory in the list file
  if (is.null(dir)) {
    dir <- lines[2]
  }

  # Check directory exists
  if (!file.exists(dir)) {
    stop("Directory does not exist.")
  }

  # The rest of the lines are the scan file names
  scan_file_names <- lines[3:length(lines)]

  # Check there are at least 3 file names (non-empty strings)
  if (length(scan_file_names) < 3 || any(scan_file_names == "")) {
    stop("There should be at least 3 non-empty scan file names.")
  }

  # Import all scans specified in the list file
  # Store them as S3 "Scan" objects in a list
  scans <- list()
  for (file_name in scan_file_names) {
    scan <- import_scan(file.path(dir, file_name))
    scans <- c(scans, list(scan))
  }

  # Sort scans by time
  scans <- scans[order(sapply(scans, function(x) x$time))]

  # Create a Run object and return
  run <- new("Run", name = scan$name, num_scans = length(scans), scans = scans)
  return(run)
}

