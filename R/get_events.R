#' Extract events from a program
#'
#' Extract events from a DHIS2 program.
#'
#' @param baseurl the server url.
#' @param program_id the program ID.
#' @param startDate the date in `YYYY-MM-DD` format.
#' @param endDate the date in `YYYY-MM-DD` format.
#' @param user string. the user account used in the pass key.
#' @param pass string. password of the user account, used in the pass key.
#' @param paging logical. Whether to page the results or not. By default, the
#'  return events are paged.
#' @param pageSize numeric. indicating the size of the page. default is `50`.
#' @param pageNumber Numeric. Specifies the page number to be returned. default is page `1`.
#' @param ... Additional params passed to [httr::config()]
#' @importFrom httr GET content authenticate timeout
#' @importFrom utils URLencode
#' @return A S3 object containing a content, endpoint and the parsed response.
#' @export
get_events <- function(baseurl = NULL, program_id = NULL,
                     startDate = NULL, endDate = NULL,
                     user = NULL, pass = NULL,
                     paging = TRUE, pageSize = 50, pageNumber = 1, ...) {
  url <- modify_events_endpoint(
    baseurl = baseurl, program_id = program_id,
    startDate = startDate, endDate = endDate,
    paging = paging, pageSize = pageSize, pageNumber = pageNumber
  )

  auth <- check_for_authentication(user, pass)

  events_res <- GET(URLencode(url),
                  ua, timeout(1000),
                  authenticate(auth$user, auth$pass),
                  config = list(...))


  check_for_response(events_res)

  parse_api_response(events_res, url, simplify_vector = T)
}

#' @importFrom utils str
print.get_events <- function(x, ...) {
  cat(sprintf("PSI-MIS <%s>\n", x$endpoint))
  str(x$content, list.len = 5, vec.len = 1)
  invisible(x)
}

#' Modify events Endpoint
#'
#' Code to modify events API Endpoint
#' @inheritParams get_events
#' @return url
#' @importFrom utils URLencode
modify_events_endpoint <- function(baseurl = NULL, program_id = NULL,
                                 startDate = NULL, endDate = NULL,
                                 paging = TRUE, pageSize = 50, pageNumber = 1) {
  if (!is.null(baseurl) && !is.null(program_id)) {
    if (!paging) {
      url <- paste0(baseurl, "api/events.json?paging=false&program=", program_id)
    } else {
      url <- paste0(baseurl, "api/events.json?paging=true&totalPages=true&program=", program_id, "&pageSize=", pageSize, "&page=", pageNumber)
    }

    if (!is.null(startDate)) {
      url <- paste0(url, "&startDate=", startDate)
    }
    if (!is.null(endDate)) {
      url <- paste0(url, "&endDate=", endDate)
    }
    URLencode(url)
  } else {
    stop("a360connect: atleast both the baseurl and the program id must be specified", call. = F)
  }
}
