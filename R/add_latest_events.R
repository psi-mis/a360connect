#' Add the latest event to a database
#'
#' Stores the latest event to a google spreadsheet database.
#'
#' @param latest_events A data.frame or data.table object with latest events.
#' @param ssid ID of a google spreadsheet.
#' @param sheet The name of the sheet in the google spreadsheet to write.
#' @param new_sheet Logical. Should a new sheet be created? by default, the
#'   latest events will be appended on the existing sheet.
#' @param overwrite logical. Do you want to overwrite data on the current sheet?
#'   default is FALSE, to append data to an existing sheet.
#' @param ... Additional params passed to the spreadsheet's endpoint
#'   \code{\link[googlesheets4:gs4_create]{gs4_create()}}
#'
#' @importFrom googlesheets4 gs4_create sheet_append
#' @importFrom data.table := .N
#' @return The input ssid, as an instance of
#'   \code{\link[googlesheets4:as_sheets_id]{as_sheets_id()}}
#' @export
add_latest_events <- function(latest_events = NULL,
                            ssid = NULL,
                            sheet = NULL,
                            new_sheet = F,
                            overwrite = F,
                            ...) {
  if (!is.null(latest_events)) {
    latest_events <- data.table::as.data.table(latest_events)
    # Add events KEYs if missing
    if (!has_key(latest_events)) {
      # Add key
      latest_events <- latest_events[
        ,
        KEY := sapply(rep(14, .N), generate_girl_uid)
      ]
      latest_events <- latest_events[
        ,
        TEI := sapply(rep(11, .N), generate_uid)
      ]
    }
  }

  if (!is.null(latest_events) && !is.null(ssid)) {
    if (overwrite) {
      ssd <- googlesheets4::sheet_write(latest_events, ss = ssid, sheet = sheet)
    } else {
      # append to an existing sheet
      if (!is.null(sheet)) {
        ssd <- googlesheets4::sheet_append(
          ss = ssid,
          latest_events, sheet = sheet
        )
      } else {
        ssd <- googlesheets4::sheet_append(ss = ssid, latest_events, sheet = 1)
      }
    }
  } else if (!is.null(latest_events) && is.null(ssid)) {
    if (new_sheet) {
      ssd <- googlesheets4::gs4_create(
        name = generate_random_code(),
        ..., sheets = latest_events
      )
    }
  } else {
    ssd <- NULL
  }
  ssd
}

#' Generate a unique girl ID
#'
#' Generate a unique girl ID to identify the events added to a database.
#'
#' Randomly generates a unique code, with three parts each separated with an
#' hyphen. XXXXXX-XYXY-00000.
#'
#' @param code_size Integer, size of the uid. default 14.
#'
#' @importFrom stats runif
#' @return A character string, the unique girl ID
#' @name generate_girl_uid
generate_girl_uid <- function(code_size = 14) {
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

#' Generate a unique DHIS2 type ID
#'
#' Randomly generates a unique DHIS2 uid.
#'
#' @param code_size Integer, size of the uid. default 14.
#' @return A character string, the unique ID
generate_uid <- function(code_size = 11) {
  runif(1)
  allowed_letters <- c(LETTERS, letters)
  allowed_chars <- c(LETTERS, letters, 0:9)
  first_char <- sample(allowed_letters, 1)
  other_chars <- sample(allowed_chars, code_size - 1)
  uid <- paste(c(first_char, paste(other_chars,
    sep = "",
    collapse = ""
  )),
  sep = "", collapse = ""
  )
  uid
}

#' Generate a random spreadsheet name
#'
#' Generate a random spreadsheet name to uniquely identify the automatically
#' generated sheets. The sheet names starts with a360connect-db, and are then
#' followed with a random code.
generate_random_code <- function() {
  runif(1)
  allowed_chars <- 0:9
  paste(
    "a360connect-db",
    paste(sample(allowed_chars, 4), collapse = "", sep = ""),
    sep = "-"
  )
}
