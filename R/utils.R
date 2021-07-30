ua <- httr::user_agent("https://github.com/psi-mis/a360connect")
timeout <- httr::timeout(60)

#' @importFrom httr http_type http_error content status_code
#' @importFrom jsonlite fromJSON
check_for_response <- function(res = NULL){
  if (!is.null(res)){
    if (http_type(res) != "application/json"){
      stop("psi-mis did not return json", call. = F)
    }
    parsed <- fromJSON(content(res, "text"), simplifyVector = F)
    if (http_error(res)){
      stop(
        sprintf("psi-mis request failed [%s] \n%s\n<%s>",
                status_code(res),
                parsed$message,
                parsed$url), call. = F
      )
    }
  }
}

check_for_authentication <- function(user = NULL, pass = NULL){
  if (is.null(user) && is.null(pass)){
    user <- Sys.getenv("C_USER")
    pass <- Sys.getenv("C_PASS")

    if (user == "" || pass == ""){
      stop("a360connect: no authenication key found, please specify")
    }
  }
  list(user = user, pass = pass)
}

#' Generate a unique girl ID
#'
#' Generate a unique identifier for girls enrolled in the follow up program.
#'
#' \code{generate_uid} generates a random id of size 15 chars.
#' @importFrom stats runif
#' @return A character string of size 15
#' @name generate_girl_uid
generate_girl_uid <- function(code_size = 14){
  runif(1)
  allowed_first_chars <- c(LETTERS, 0:9)
  allowed_middle_letters <- LETTERS
  allowed_last_chars <- 0:9
  first_part <- sample(allowed_first_chars, 6)
  middle_part <- sample(allowed_middle_letters, 4)
  last_part <- sample(allowed_last_chars, code_size - 10)
  uid <- paste(
    paste0(first_part, collapse = ""),
    paste0(middle_part, collapse = ""),
    paste0(last_part, collapse = ""),
    sep = "-"
  )
  uid
}

#' Generate object unique ID
#'
#' @rdname generate_girl_uid
generate_uid <- function(code_size = 11){
  runif(1)
  allowedLetters <- c(LETTERS, letters)
  allowedChars <- c(LETTERS, letters, 0:9)
  firstChar <- sample(allowedLetters, 1)
  otherChars <- sample(allowedChars, codeSize - 1)
  uid <- paste(c(firstChar, paste(otherChars, sep = "", collapse = "")), sep = "", collapse = "")
  uid
}

generate_random_code <- function(){
  runif(1)
  allowed_chars <- 0:9
  paste(
    "a360connect-db",
    paste(sample(allowed_chars, 4), collapse = "", sep = ""),
    sep = "-"
  )
}

required_fields <- c("event","eventDate","orgUnit","orgUnitName","status","Name of girl","Girl ID","Age of Girl",
                     "Phone Number","Provider's name","Newly registered client","Visit Type (First/Follow-up/Repeat)",
                     "Date of Service Provision","Method taken up", "Follow-up scheduled (date)","Date of follow up call")

has_data_values <- function(events = NULL){
  if (!is.null(events) && "dataValues" %in% names(events)){
    TRUE
  } else{
    FALSE
  }
}

has_key <- function(dt) ifelse(any(names(dt) == "KEY"), TRUE, FALSE)

#' Check if the any record has a phone number
#'
#' @importFrom data.table is.data.table .N
#' @return Logical
has_phone_number <- function(events){
  if (is.data.table(events)){
    events[!is.na(events$`Phone Number`) & nchar(events$`Phone Number`) >= 10, .N > 0]
  }else{
    any(!is.na(events$`Phone Number`) & nchar(events$`Phone Number`) >= 10)
  }
}

#' Check if any record has a duplicate name
#' @param events A data.frame object
#' @return Logical
has_duplicate_names <- function(events){
  girl_names <- tolower(
    stringr::str_squish(events$`Name of girl`)
  )
  any(duplicated(girl_names))
}

review_names <- function(x){
  x <- stringr::str_to_lower(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_trim(x, side = "both")
  x
}

has_KEY <- function(evts){
  if ("KEY" %in% names(evts)){
    if (is.data.table(evts)){
      evts[!is.na(`KEY`), .N > 0]
    } else {
      any(!is.na(evts$`KEY`))
    }
  } else{
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
review_search_result <- function(evts){

  # is not empty
  evts <- evts[!is_empty(evts)]

  if (length(evts) > 0){
    evts <- data.table::rbindlist(evts)
    evts <- unique(evts)
    evts
  } else{
    NULL
  }
}

