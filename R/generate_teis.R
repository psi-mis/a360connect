#' Generate TEIs from A360 Events Program
#'
#' Generate Tracked Entity Instances (TEIs) from A360 Attendance & Service Log,
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
#' @name generate_teis
#' @export
generate_teis <- function(baseurl = NULL, program_id = NULL, warn_duplicates = T){
  # to do

}

#'Pull Events
#'@importFrom httr GET content
#'@importFrom utils URLencode
#'@inheritParams generate_teis
pull_events <- function(baseurl = NULL, program_id = NULL, startDate = NULL, endDate = NULL){
  url <- events_endpoint(baseurl = baseurl, program_id = program_id, startDate = startDate, endDate = endDate)

  events_res <- GET(URLencode(url), ua, timeout)

  check_response(events_res)

  parsed_events <- fromJSON(
   content(res,"text"),
   simplifyVector = T
   )

  structure(
    list(content = parsed_events,
         endpoint = url,
         response = resp), class ="psi-mis_api"
  )

}

print.pull_events <- function(x, ...){
  cat(sprintf("PSI-MIS <%s>", x$endpoint))
  head(x$content$events, 10)
  invisible(x)
}


#' Transform event data values
#'
#' @param events
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

  dplyr::bind_rows(transformed_datavalues)
}




events_endpoint <- function(baseurl = NULL, program_id = NULL, startDate = NULL, endDate = NULL){
  if (!is.null(baseurl) && !is.null(program_id)){
    url <- paste0(baseurl, "api/events?paging=false&program=", program_id)
    if (!is.null(startDate)){
      url <- paste0(url,startDate, collapse = "&")
    }
    if (!is.null(endDate)){
      url <- paste0(url,endDate, collapse = "&")
    }
    url
  }else{
    stop("a360connect: atleast both the baseurl and the program id must be specified", call. = F)
  }
}

