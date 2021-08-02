#'Extract evnt from a program
#'
#'Extract evnt from a DHIS2 program.
#'
#'@param baseurl the server url.
#'@param program_id the program ID.
#'@param startDate the date in `YYYY-MM-DD` format.
#'@param endDate the date in `YYYY-MM-DD` format.
#'@param user string. the user account used in the pass key.
#'@param pass string. password of the user account, used in the pass key.
#'@param paging logical. Whether to page the results or not. By default, the
#'  return evnt are paged.
#'@param pageSize numeric. indicating the size of the page. default is `50`.
#'@importFrom httr GET content authenticate
#'@importFrom utils URLencode
#'@return A S3 object containing a content, endpoint and the parsed response.
#'@export
get_evnt <- function(baseurl = NULL, program_id = NULL,
                        startDate = NULL, endDate = NULL,
                        user = NULL, pass = NULL,
                        paging = TRUE, pageSize = 50, ...){

  url <- modify_evnt_endpoint(baseurl = baseurl, program_id = program_id,
                                startDate = startDate, endDate = endDate,
                                paging = paging, pageSize = pageSize)

  auth <- check_for_authentication(user, pass)

  evnt_res <- GET(URLencode(url), ua, timeout, authenticate(auth$user,auth$pass), config = list(...))

  check_for_response(evnt_res)

  parsed_evnt <- fromJSON(
    content(evnt_res,"text"),
    simplifyVector = T
  )

  structure(
    list(content = parsed_evnt,
         endpoint = url,
         response = evnt_res), class ="psi-mis_api"
  )
}

#' @importFrom utils str
print.get_evnt <- function(x, ...){
  cat(sprintf("PSI-MIS <%s>\n", x$endpoint))
  str(x$content, list.len = 5, vec.len = 1)
  invisible(x)
}





