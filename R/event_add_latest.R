#' Add the latest events to a database
#'
#' Stores the latest events to a database. Currently, it stores the records in a
#' google spreadsheet.
#'
#' @param latest_events A data.frame object.
#' @param ssid the ID of a google spreadsheet.
#' @param sheet the name of the sheet
#' @param new_sheet logical. Create a new sheet? default is FALSE, to append
#'   data to an existing spreadsheet.
#' @param overwrite logical. Do you want to overwrite data on the current sheet?
#'   default is FALSE, to append data to an existing sheet.
#' @param ... Additional params passed to the spreadsheet's endpoint
#'   \ref{googlespreadsheet4::gs4_create}.
#' @importFrom googlesheets4 gs4_create sheet_append
#' @importFrom data.table := .N
#' @return
#' @export
add_latest_events <- function(latest_events = NULL, ssid = NULL, sheet = NULL, new_sheet = F, overwrite = F, ...){
  if (!is.null(latest_events)){
    latest_events <- data.table::as.data.table(latest_events)
    # Add event KEYs if missing
    if (!has_key(latest_events)){
      # Add key
      latest_events <- latest_events[, KEY := sapply(rep(14, .N), generate_girl_uid)]
    }

  }

  if (!is.null(latest_events) && !is.null(ssid)){

    if (overwrite){
      ssd <- googlesheets4::sheet_write(latest_events,ss = ssid, sheet = sheet)
    } else {
      # append to an existing sheet
      if (!is.null(sheet)){
        ssd <- googlesheets4::sheet_append(ss = ssid,latest_events, sheet = sheet)
      } else{
        ssd <- googlesheets4::sheet_append(ss = ssid,latest_events, sheet = 1)
      }
    }
  } else if (!is.null(latest_events) && is.null(ssid)){
    if (new_sheet){
      ssd <- googlesheets4::gs4_create(name = generate_random_code(), ..., sheets = latest_events)
    }
  } else{
    ssd <- NULL
  }
  ssd
}
