#' Transform events
#'
#' Transform events data values from long to wide, and selects the required
#' fields for follow-up.
#'
#' @detail In case the events are missing data values, only the
#' required fields are selected.
#'
#' @param events A data.frame object with events to transform.
#' @importFrom dplyr any_of
#' @export
transform_events <- function(events) {

  output_progress("transforming the events",
                  cli_fun = "cli_h3")

  if (has_data_values(events)) {
    # flatten the data values to select the required fields
    output_progress("flattening the data values",
                    cli_fun = "cli_alert_info")

    new_datavalues <- pb_lapply(events$dataValues, function(x) {
      if (!is.null(x) && length(x) > 0) {
        # transform the data values only if a data element column exist
        if ("dataElement" %in% names(x)){
          datavalues <- tidyr::pivot_wider(x, names_from = dataElement, values_from = value)
        } else{
          NULL
        }

      } else {
        NULL
      }
    })
    events$dataValues2 <- new_datavalues

    # Add the event, date, and, status, orgUnit id and name into the data values to easily identify the records
    output_progress("adding relevant event details to the records",
                    cli_fun = "cli_alert_info")

    events_split <- split(events, as.numeric(row.names(events)))
    transformed_datavalues <- pb_lapply(events_split, function(x) {
      if (!is.null(x$dataValues2[[1]])) {
        dplyr::mutate(x$dataValues2[[1]],
          orgUnit = x$orgUnit,
          orgUnitName = x$orgUnitName,
          event = x$event,
          eventDate = x$eventDate,
          status = x$status
        )
      } else {
        NULL
      }
    })

    dv <- dplyr::bind_rows(transformed_datavalues)
    # rename
    names(dv) <- plyr::mapvalues(names(dv),
      from = des$id,
      to = des$name, warn_missing = F
    )
    # select the required filled
    # required_fields <- c("event","eventDate","orgUnit","orgUnitName","status","Name of girl","Girl ID","Age of Girl",
    #                      "Phone Number","Provider's name","Newly registered client","Visit Type (First/Follow-up/Repeat)",
    #                      "Date of Service Provision","Method taken up", "Follow-up scheduled (date)","Date of follow up call")
    dv <- dplyr::select(dv, any_of(required_fields))
    dv
  } else {
    dplyr::select(events, any_of(required_fields))
  }
}

required_fields <- c(
  "event", "eventDate", "orgUnit", "orgUnitName", "status", "Name of girl", "Girl ID", "Age of Girl",
  "Phone Number", "Provider's name", "Newly registered client", "Visit Type (First/Follow-up/Repeat)",
  "Date of Service Provision", "Method taken up", "Follow-up scheduled (date)", "Date of follow up call"
)
