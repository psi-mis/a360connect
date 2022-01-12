#' Find latest
#'
#' Find the latest A360 event for follow up from a list of service provision
#' reports stored in a google spreadsheet.
#'
#' @details The latest event is determined using the `girl ID`, `name of girl`,
#' and `phone number` for girls with a single report and `date of service
#' provision` for girls with multiple reports.
#'
#' In cases where there are multiple reports for the same girl, but the newest
#' is missing the phone number, the newest record is updated with any available
#' phone number in the reports:
#'
#' * record 1 phone number : NA
#'
#' * record 2 phone number : XXX XXX XXXX
#'
#' * record 3 phone number : NA
#'
#' the newest record will be assigned the phone number : XXX XXX XXXX
#'
#' this is similar to cases where there different phone numbers:
#'
#' * record 1 phone number : NA
#'
#' * record 2 phone number : XXX XXX XXXX
#'
#' * record 3 phone number : YYY XXX YYYY
#'
#' the newest record will be assigned both available phone numbers : XXX XXX
#' XXXX | YYY XXX YYYY
#'
#' the phone number is validated; only records with a phone number equal to or
#' more than `10 digits` are returned.
#' @name find_latest
#' @return A data.table object with the latest unique events for follow up.
NULL


#' @describeIn find_latest Find the latest event for follow up
#'
#' @param events A data frame with events to find.
#' @param from A google spreadsheet ID, where to search the events from.
#' @param ... Additional params passed to the \link[googlesheets4]{read_sheet}.
#' @importFrom data.table as.data.table rbindlist
#' @export
find_latest <- function(events, from = NULL, ...) {
  if (!is.null(from)) {
    sheet_params <- list(...)
    events <- googlesheets4::read_sheet(ss = from, col_types = "c", sheet_params)
  }

  events <- data.table::as.data.table(events)

  events <- find_latest_by_name_and_id(events)

  events <- find_latest_by_phone_number(events)

  events
}

#' @describeIn find_latest Find the latest event by girl id and name
#' @importFrom data.table is.data.table
find_latest_by_name_and_id <- function(events) {
  if (!is.data.table(events)) {
    events <- as.data.table(events)
  }

  events_ls <- lapply(events$`Girl ID`, function(x) {
    # filter by girl id
    events_by_id <- events[`Girl ID` == x, ]

    if (events_by_id[, .N] == 1 && has_phone_number(events_by_id)) {
      # return the unique and latest
      events_by_id
    } else if (events_by_id[, .N] > 1 && has_phone_number(events_by_id)) {
      # check for duplicates names and return latest
      if (has_duplicate_names(events_by_id) && has_phone_number(events_by_id)) {
        # get latest unique events
        find_latest_by_name(events_by_id)
      } else if (!has_duplicate_names(events_by_id) && has_phone_number(events_by_id)) {
        # these are unique clients, its by coincidence they have similar names
        events_by_id[!is.na(`Phone Number`) & nchar(`Phone Number`) >= 10, ]
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  review_search_result(events_ls)
}


#' @describeIn find_latest Find latest the event by name
find_latest_by_name <- function(events) {
  if (!is.data.table(events)) {
    events <- as.data.table(events)
  }

  events <- events[, girl_name := review_names(`Name of girl`)]

  events_ls <- lapply(unique(events$girl_name), function(x) {
    dt <- events[girl_name == x, ]
    if (dt[, .N] == 1 && has_phone_number(dt)) {
      dt[, -"girl_name"]
    } else if (dt[, .N] > 1 && has_phone_number(dt)) {
      # review and overwrite the phone numbers
      phone_number <- dt[!is.na(`Phone Number`) & nchar(`Phone Number`) >= 10, `Phone Number`]
      phone_number <- unique(phone_number)
      if (length(phone_number) > 1) {
        phone_number <- paste(phone_number, collapse = " | ")
      }

      dt <- dt[, `Phone Number` := rep(phone_number, .N)]

      # copy keys
      if (has_KEY(dt)) {
        key <- dt[!is.na(`KEY`), `KEY`]
        if (length(key) > 1) {
          stop(sprintf(
            "Multiple KEYS found! \n
            <%s>", paste(key, collapse = "; ")
          ), call. = F)
        }
        # overwrite key
        dt <- dt[, KEY := rep(key, .N)]
      }

      # sort the Date of Service Provision in descending order
      dt <- dt[order(-`Date of Service Provision`)]
      # latest
      dt[1, -"girl_name"]
    } else {
      NULL
    }
  })

  review_search_result(events_ls)
}

#' @describeIn find_latest Find the latest event by phone number
find_latest_by_phone_number <- function(events) {
  if (!is.data.table(events)) {
    events <- data.table::as.data.table(events)
  }
  events_ls <- lapply(events$`Phone Number`, function(x) {
    d <- events[`Phone Number` == x]

    if (d[, .N] == 1) {
      d
    } else if (d[, .N] > 1) {
      d <- d[order(-`Date of Service Provision`)]
      d[1, ]
    } else {
      NULL
    }
  })
  review_search_result(events_ls)
}
