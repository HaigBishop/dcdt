#' Plot AUC data.
#'
#' @param run The S4 "Run" object which holds all data from an AUC run.
#' @param start The start index to be plotted.
#' @param end The end index to be plotted.
#' @param step The step size for selecting data between start and end.
#' @param radius_range The range of radii to be plotted.
#'
#' @return No return value. This function is used for plotting purposes.
#' @export
#'
#' @import ggplot2
#' @examples
#' \dontrun{
#' # plot every second piece of the raw data from run_1.
#' plot_raw(run_1, step=2)
#'
#' # plot all the raw data from run_2, between radius values of 6.25 and 7.1
#' plot_raw(run_2, radius_range=c(6.25, 7.1))
#' }
#'
plot_raw <- function(run, start = 1, end = NULL, step = 1, radius_range = NULL) {
  # Check the input is a Run class
  if (!is(run, "Run")) {
    stop("Input must be a Run object.")
  }

  # Set the end point if not provided
  if (is.null(end)) {
    end <- run@num_scans

  # Throw error if end is too big or too small
  } else if (end > run$num_scans) {
    stop("end mustn't be greater than the number of scans.")
  } else if (end < 1) {
    stop("end must be at least 1.")
  }

  # Set the radius range if not provided
  if (is.null(radius_range)) {
    # Collect all radii from all scans
    radii <- vector("numeric", length = 0)
    for (i in seq(start, end, by = step)) {
      radii <- c(run@scans[[i]]["data"]$data[["radius"]], radii)
    }
    # Use max and min radii from all radii from all scans
    radius_range <- c(min(radii), max(radii))}


  # Collect all radii, x values, and y values from all scans
  # But, only between the specified radii
  radii <- vector("numeric", length = 0)
  xs <- vector("numeric", length = 0)
  ys <- vector("numeric", length = 0)
  for (i in seq(start, end, by = step)) {
    trimmed_df <- subset(run@scans[[i]]["data"]$data, radius >= radius_range[1] & radius <= radius_range[2])
    radii <- c(trimmed_df[["radius"]], radii)
    xs <- c(trimmed_df[["x"]], xs)
    ys <- c(trimmed_df[["y"]], ys)
  }

  # Create an empty initial plot
  plot(1, type = "n", xlim = radius_range, ylim = c(min(xs), max(xs)), xlab = "Radius", ylab = "Absorbance", main = run@name)

  # Loop through each scan from start to end using the step
  for (i in seq(start, end, by = step)) {
    # Grab the data frame and add this scan to the plot
    df <- run@scans[[i]]["data"]$data
    lines(df[["radius"]], df[["x"]], type = "l", col = "blue")
  }
}
