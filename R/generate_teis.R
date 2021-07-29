#' Generate TEIs from A360 Events Program
#'
#' @description Generate Tracked Entity Instances (TEIs) from A360 Attendance & Service Log,
#' an event program.
#' @details \code{generate_teis} pulls A360 events from the Attendance & Service Log,
#' checks for the latest unique events, and converts them into tracked entity
#' instances for follow up.
#'
#' @param baseurl the server url.
#' @param program_id the ID of A360 Attendance & Service Log program.
#' @param warn_duplicates logical. Whether to highlight duplicates or not. Default is TRUE.
#' @return An S3 object of the generated TEIs with possible duplicates if found.
#' @examples
#' generate_teis(baseurl = "", program_id = "x")
#' @export
#' @noRd
generate_teis <- function(baseurl = NULL, program_id = NULL, warn_duplicates = T){
  # to do

}











