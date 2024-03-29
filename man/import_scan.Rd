% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_scan.R
\name{import_scan}
\alias{import_scan}
\title{Import raw data from an AUC scan into R}
\usage{
import_scan(scan_path)
}
\arguments{
\item{scan_path}{The location of the file containing the AUC scan data.}
}
\value{
The AUC data from the scan in the form of an S3 "Scan" object.
}
\description{
This function reads a "scan file" which contains the data from a particular
scan in an AUC run.
This function is employed by dcdt::import_run to import all scans in the run.
The data in the scan files is the name (on line 1), the scan parameters
(on line 2), and the data itself (on all following lines).
This data is returned in the form of a S3 "Scan" object.
}
\examples{
\dontrun{
# Load the scan in scan_file.RA2
auc_scan_1 <- dcdt::import_scan("path/to/your/scan_file.RA2")
}

}
