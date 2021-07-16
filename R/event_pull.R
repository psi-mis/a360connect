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

print.pull_events <- function(x, ...){
  cat(sprintf("PSI-MIS <%s>", x$endpoint))
  head(x$content$events, 10)
  invisible(x)
}
