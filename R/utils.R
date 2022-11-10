ua <- httr::user_agent("https://github.com/psi-mis/a360connect")
timeout <- httr::timeout(2000)

#' Review an API response
#'
#' Check that API returned a `JSON` element without any errors.
#'
#' @param res A DHIS2 response object.
#' @importFrom httr http_type http_error content status_code
#' @importFrom jsonlite fromJSON
check_for_response <- function(res = NULL) {
  if (!is.null(res)) {
    if (http_type(res) != "application/json") {
      stop(sprintf("psi-mis did not return json \n<%s>", res)
        , call. = F)
    }
    parsed <- fromJSON(content(res, "text"), simplifyVector = F)
    if (http_error(res)) {
      stop(
        sprintf(
          "psi-mis request failed [%s] \n%s\n<%s>",
          status_code(res),
          parsed$message,
          parsed$url
        ),
        call. = F
      )
    }
  }
}

#' Review API authentication
#'
#' Check that the API pass key is specified. It looks for the API key from the
#' `r` `environment`, if not supplied, otherwise throws an error.
#'
#' @param user A DHIS2 user account to authenticate.
#' @param pass Password of the DHIS2 Account.
#' @return A list with API key (user and pass) if found.
check_for_authentication <- function(user = NULL, pass = NULL) {
  if (is.null(user) && is.null(pass)) {
    user <- Sys.getenv("C_USER")
    pass <- Sys.getenv("C_PASS")

    if (user == "" || pass == "") {
      stop("a360connect: no authenication key found, please specify")
    }
  }
  list(user = user, pass = pass)
}

#' Check if an event has data values
#'
#' @param events A data.frame or data table object. Events to search.
#' @return logical.
#' @noRd
has_data_values <- function(events = NULL) {
  if (!is.null(events) && "dataValues" %in% names(events)) {
    TRUE
  } else {
    FALSE
  }
}

#' Does the event have any key?
#'
#' @param dt A data.frame or data table object. Events to search
#' @return logical.
#' @noRd
has_key <- function(dt) ifelse(any(names(dt) == "KEY"), TRUE, FALSE)

#' Check if the any record has a phone number
#'
#' @param events A data.frame or data table object. Events to search.
#' @importFrom data.table is.data.table .N
#' @return Logical
#' @noRd
has_phone_number <- function(events) {
  if (is.data.table(events)) {
    events[!is.na(events$`Phone Number`) & nchar(events$`Phone Number`) >= 10, .N > 0]
  } else {
    any(!is.na(events$`Phone Number`) & nchar(events$`Phone Number`) >= 10)
  }
}

#' Check if any record has a duplicate name
#' @param events A data.frame object
#' @return Logical
has_duplicate_names <- function(events) {
  girl_names <- tolower(
    stringr::str_squish(events$`Name of girl`)
  )
  any(duplicated(girl_names))
}

review_names <- function(x) {
  x <- stringr::str_to_lower(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_trim(x, side = "both")
  x
}

has_KEY <- function(evts) {
  if ("KEY" %in% names(evts)) {
    if (is.data.table(evts)) {
      evts[!is.na(`KEY`), .N > 0]
    } else {
      any(!is.na(evts$`KEY`))
    }
  } else {
    FALSE
  }
}

is_empty <- function(x) vapply(x, is.null, logical(1))

#' Review search results
#'
#' Combine results and return unique events
#'
#' @param evts A list of data.table, the events return from search
#' @importFrom data.table rbindlist
#' @return reviewed result, A data.table if
review_search_result <- function(evts) {

  # is not empty
  evts <- evts[!is_empty(evts)]

  if (length(evts) > 0) {
    evts <- data.table::rbindlist(evts)
    evts <- unique(evts)
    evts
  } else {
    NULL
  }
}

#' Parse an API response
#'
#' @param res the API response
#' @param url the endpoint url
#' @param simplify_vector passed to [jsonlite::fromJSON()]
#' @param name class name of the S3 object
#' @return S3 object
parse_api_response <- function(res, url, simplify_vector = F, name = NULL) {
  d <- content(res, "text")
  d <- fromJSON(d, simplifyVector = simplify_vector)


  if (is.null(name)) {
    name <- "psi-mis_api"
  }

  structure(
    list(
      content = d,
      endpoint = url,
      response = res
    ),
    class = name
  )
}

#' Adapt lapply function
#'
#' Adapt lapply function to show progress bar, and provide a default fallback if
#' pbapply is not loaded.
#'
#' @noRd
pb_lapply <- function(x, fun, ...) {
  if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pblapply(x, fun, ...)
  } else {
    lapply(x, fun, ...)
  }
}

clear_names <- function(x) paste0(x)

is_tei_payload <- function(payload){
  if (all(
    c("trackedEntityType","trackedEntityInstance",
      "orgUnit","attributes","enrollments") %in% names(payload))){
    TRUE
  } else{
    FALSE
  }
}

has_incident_date <- function(df){
  if ("incident_date" %in% names(df)){
    TRUE
  } else {
    FALSE
  }
}

has_enrollment_date <- function(df){
  if ("enrollment_date" %in% names(df)){
    TRUE
  } else {
    FALSE
  }
}

has_tei_ids <- function(df){
  if ("TEI" %in% names(df)){
    TRUE
  } else {
    FALSE
  }
}

output_progress <- function(msg = "message", cli_fun = "cli_alert" , crayon_fun = NULL){

  if (!is.null(crayon_fun)){
    txt <- paste0(
      "cli::", cli_fun, "(",
      "crayon::", crayon_fun, "('",
      msg,
      "'))"
    )
  } else {
    txt <- paste0(
      "cli::", cli_fun, "('",
      msg,
      "')"
    )
  }

  eval(parse(text = txt))
}


