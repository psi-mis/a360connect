#' Transform event data values
#'
#' @param events A data.frame object.
#' @importFrom dplyr any_of
#' @export
transform_events <- function(events){

  if (has_data_values(events)){
    # flatten the data values
    new_datavalues <- purrr::map(events$dataValues, function(x){
      if (!is.null(x) && length(x) > 0){
        datavalues <- tidyr::pivot_wider(x, id_cols = dataElement, names_from = dataElement, values_from = value)
      } else{
        NULL
      }
    })
    events$dataValues2 <- new_datavalues

    # # remove any nulls
    # events <- events[!vapply(events$dataValues2, is.null, logical(1))]

    # Add the event, date, and, status, orgUnit id and name into the data values
    events_split <- split(events, as.numeric(row.names(events)))
    transformed_datavalues <- purrr::map(events_split, function(x){
      if (!is.null(x$dataValues2[[1]])){
        dplyr::mutate(x$dataValues2[[1]],
                      orgUnit = x$orgUnit,
                      orgUnitName = x$orgUnitName,
                      event = x$event,
                      eventDate = x$eventDate,
                      status = x$status)
      } else{
        NULL
      }

    })

    dv <- dplyr::bind_rows(transformed_datavalues)
    # rename
    names(dv) <- plyr::mapvalues(names(dv),
                                 from = des$id,
                                 to = des$name, warn_missing = F)
    # select the required filled
    # required_fields <- c("event","eventDate","orgUnit","orgUnitName","status","Name of girl","Girl ID","Age of Girl",
    #                      "Phone Number","Provider's name","Newly registered client","Visit Type (First/Follow-up/Repeat)",
    #                      "Date of Service Provision","Method taken up", "Follow-up scheduled (date)","Date of follow up call")
    dv <- dplyr::select(dv, any_of(required_fields))
    dv
  } else{
    dplyr::select(events, any_of(required_fields))
  }

}
