#' Add the latest event to a database
#'
#' Stores the latest event to a google spreadsheet database.
#'
#' @param latest_evnt A data.frame or data.table object with latest events.
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
add_latest_evnt <- function(latest_evnt = NULL, ssid = NULL, sheet = NULL, new_sheet = F, overwrite = F, ...) {
  if (!is.null(latest_evnt)) {
    latest_evnt <- data.table::as.data.table(latest_evnt)
    # Add evnt KEYs if missing
    if (!has_key(latest_evnt)) {
      # Add key
      latest_evnt <- latest_evnt[, KEY := sapply(rep(14, .N), generate_girl_uid)]
      latest_evnt <- latest_evnt[, TEI := sapply(rep(11, .N), generate_uid)]
    }
  }

  if (!is.null(latest_evnt) && !is.null(ssid)) {
    if (overwrite) {
      ssd <- googlesheets4::sheet_write(latest_evnt, ss = ssid, sheet = sheet)
    } else {
      # append to an existing sheet
      if (!is.null(sheet)) {
        ssd <- googlesheets4::sheet_append(ss = ssid, latest_evnt, sheet = sheet)
      } else {
        ssd <- googlesheets4::sheet_append(ss = ssid, latest_evnt, sheet = 1)
      }
    }
  } else if (!is.null(latest_evnt) && is.null(ssid)) {
    if (new_sheet) {
      ssd <- googlesheets4::gs4_create(name = generate_random_code(), ..., sheets = latest_evnt)
    }
  } else {
    ssd <- NULL
  }
  ssd
}
