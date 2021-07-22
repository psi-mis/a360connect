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




#' Transform event data values
#'
#' @param events A data.frame object.
transform_event_datavalues <- function(events){
  # flatten the data values
  new_datavalues <- purrr::map(events$dataValues, function(x){
    if (!is.null(x) && length(x) > 0){
      datavalues <- tidyr::pivot_wider(x, id_cols = dataElement, names_from = dataElement, values_from = value)
    }
  })
  events$dataValues2 <- new_datavalues

  # Add the event, date, and, status, orgUnit id and name into the data values
  events_split <- split(events, as.numeric(row.names(events)))
  transformed_datavalues <- purrr::map(events_split, function(x){
    dplyr::mutate(x$dataValues2[[1]],
                  orgUnit = x$orgUnit,
                  orgUnitName = x$orgUnitName,
                  event_id = x$event,
                  eventDate = x$eventDate,
                  status = x$status)
  })

  dv <- dplyr::bind_rows(transformed_datavalues)
  # rename
  names(dv) <- plyr::mapvalues(names(dv),
                               from = des$id,
                               to = des$name, warn_missing = F)
  dv
}









