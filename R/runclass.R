#' Definition of an S4 Class for an AUC run.
#'
#' @slot name character. The name of the run.
#' @slot num_scans integer. The number of scans in the run.
#' @slot scans list. A list of all the scans in the run (S3 "Scan" objects).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' Create a Run object
#' run <- new("Run", name = scan$name, num_scans = length(scans), scans = scans)
#' }
#'
Run <- setClass("Run", representation(name = "character", num_scans = "integer", scans = "list"))
