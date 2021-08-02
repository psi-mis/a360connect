#' Add the latest evnt to a database
#'
#' Stores the latest evnt to a database. Currently, it stores the records in a
#' google spreadsheet.
#'
#' @param latest_evnt A data.frame object.
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
add_latest_evnt <- function(latest_evnt = NULL, ssid = NULL, sheet = NULL, new_sheet = F, overwrite = F, ...){
  if (!is.null(latest_evnt)){
    latest_evnt <- data.table::as.data.table(latest_evnt)
    # Add evnt KEYs if missing
    if (!has_key(latest_evnt)){
      # Add key
      latest_evnt <- latest_evnt[, KEY := sapply(rep(14, .N), generate_girl_uid)]
    }

  }

  if (!is.null(latest_evnt) && !is.null(ssid)){

    if (overwrite){
      ssd <- googlesheets4::sheet_write(latest_evnt,ss = ssid, sheet = sheet)
    } else {
      # append to an existing sheet
      if (!is.null(sheet)){
        ssd <- googlesheets4::sheet_append(ss = ssid,latest_evnt, sheet = sheet)
      } else{
        ssd <- googlesheets4::sheet_append(ss = ssid,latest_evnt, sheet = 1)
      }
    }
  } else if (!is.null(latest_evnt) && is.null(ssid)){
    if (new_sheet){
      ssd <- googlesheets4::gs4_create(name = generate_random_code(), ..., sheets = latest_evnt)
    }
  } else{
    ssd <- NULL
  }
  ssd
}
