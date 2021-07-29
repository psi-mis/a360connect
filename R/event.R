#'Extract events from a program
#'
#'Extract events from a DHIS2 program.
#'
#'@param baseurl the server url.
#'@param program_id the program ID.
#'@param startDate the date in `YYYY-MM-DD` format.
#'@param endDate the date in `YYYY-MM-DD` format.
#'@param user string. the user account used in the pass key.
#'@param pass string. password of the user account, used in the pass key.
#'@param paging logical. Whether to page the results or not. By default, the
#'  return events are paged.
#'@param pageSize numeric. indicating the size of the page. default is `50`.
#'@importFrom httr GET content authenticate
#'@importFrom utils URLencode
#'@return A S3 object containing a content, endpoint and the parsed response.
#'@export
pull_events <- function(baseurl = NULL, program_id = NULL,
                        startDate = NULL, endDate = NULL,
                        user = NULL, pass = NULL,
                        paging = TRUE, pageSize = 50, ...){

  url <- modify_events_endpoint(baseurl = baseurl, program_id = program_id,
                                startDate = startDate, endDate = endDate,
                                paging = paging, pageSize = pageSize)

  auth <- check_for_authentication(user, pass)

  events_res <- GET(URLencode(url), ua, timeout, authenticate(auth$user,auth$pass), config = list(...))

  check_for_response(events_res)

  parsed_events <- fromJSON(
    content(events_res,"text"),
    simplifyVector = T
  )

  structure(
    list(content = parsed_events,
         endpoint = url,
         response = events_res), class ="psi-mis_api"
  )
}

#' @importFrom utils str
print.pull_events <- function(x, ...){
  cat(sprintf("PSI-MIS <%s>\n", x$endpoint))
  str(x$content, list.len = 5, vec.len = 1)
  invisible(x)
}

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




