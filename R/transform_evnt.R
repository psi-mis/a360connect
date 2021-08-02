#' Transform evnt data values
#'
#' @param evnt A data.frame object.
#' @importFrom dplyr any_of
#' @export
transform_evnt <- function(evnt){

  if (has_data_values(evnt)){
    # flatten the data values
    new_datavalues <- purrr::map(evnt$dataValues, function(x){
      if (!is.null(x) && length(x) > 0){
        datavalues <- tidyr::pivot_wider(x, id_cols = dataElement, names_from = dataElement, values_from = value)
      } else{
        NULL
      }
    })
    evnt$dataValues2 <- new_datavalues

    # # remove any nulls
    # evnt <- evnt[!vapply(evnt$dataValues2, is.null, logical(1))]

    # Add the evnt, date, and, status, orgUnit id and name into the data values
    evnt_split <- split(evnt, as.numeric(row.names(evnt)))
    transformed_datavalues <- purrr::map(evnt_split, function(x){
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
    dplyr::select(evnt, any_of(required_fields))
  }

}
