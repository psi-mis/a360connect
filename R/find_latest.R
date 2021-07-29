#' Find latest events
#'
#' Identify the latest events for follow-up.
#'
#' Reviews an entire list of events and identify the latets unique events.
#'
#' @param events A dataframe.
#' @return A data frame of the latest unique record
#' @importFrom data.table as.data.table rbindlist
#' @export
find_latest_event <- function(events){
  events_dt <- data.table::as.data.table(events)
  events_filtered <- purrr::map(events_dt$`Girl ID`, function(x){

    # filter by girl ID
    events_dt_by_id <- events_dt[`Girl ID` == x,]

    # Get the new unique client; first time users
    if (nrow(events_dt_by_id) == 1 && has_phone_number(events_dt_by_id)){
      events_dt_by_id
    }else if (nrow(events_dt_by_id) > 1 && has_phone_number(events_dt_by_id)){ # Treat others as duplicates
      # check duplicates by girl name & harmonize the records
      # return unique or the latest client record
      if (has_duplicate_names(events_dt_by_id)){
        # add the unique names (girl_id)
        events_dt_by_id1 <- dplyr::mutate(events_dt_by_id,
                                    girl_name = tolower(
                                      stringr::str_squish(events_dt_by_id$`Name of girl`)
                                    )
                                  )

        # filter by unique names
        events_dt_by_id1 <- purrr::map_df(unique(events_dt_by_id1$girl_name), function(x){
          #dt <- dplyr::filter(events_dt_by_id1, girl_name == x)
          dt <- events_dt_by_id1[girl_name == x]

          if (nrow(dt) == 1 && has_phone_number(dt)){ # return row if has a phone number
            #dt_unique  <- dplyr::select(dt, -`girl_name`)
            dt_unique <- dt[, -"girl_name"]
            dt_unique
          }else if (nrow(dt) > 1 && has_phone_number(dt)){ # if more than on e rwo is returned, overwrite the Phone Numbers
            phone_number <- dt[!is.na(`Phone Number`) & nchar(`Phone Number`) >= 10, `Phone Number` ]
            if (length(phone_number) > 1){
              phone_number <- paste(phone_number, collapse = " | ")
            }
            # Overwrite the phone number
            #dt <- dplyr::mutate(dt, `Phone Number` = rep(phone_number, nrow(dt)))
            dt <- dt[,`Phone Number`:= rep(phone_number, .N)]

            # sort the Date of Service Provision in descending order
            #dt <- dplyr::arrange(dt, desc(as.Date(`Date of Service Provision`)))
            dt<- dt[order(-`Date of Service Provision`)]

            # deselect the girl_name
            # and return the first item
            #dt <- dplyr::select(dt, -`girl_name`)
            dt <- dt[, -"girl_name"]
            dt[1,]
            # dt_latest <- dt[1,]
            # dt_latest
          } else {
            NULL
          }

        })

        events_dt_by_id1

      }else if (!has_duplicate_names(events_dt_by_id) && has_phone_number(events_dt_by_id)){
        # these are unique clients, its just by coincidence they have similar names
        # filter those with phone numbers
        events_dt_by_id[!is.na(`Phone Number`) & nchar(`Phone Number`) >= 10,]
      }else{
        NULL
      }
    }else {
      NULL
    }

  })

  latest <- rbindlist(events_filtered)
  latest <- unique(latest)
  latest <- latest[nchar(`Phone Number`) >= 10] # remove the 0s or 1s

  # check for any duplicates by Phone number
  latest_events <- lapply(latest$`Phone Number`, function(x){
    latest_event <- latest[`Phone Number` == x]

    if (latest_event[,.N] == 1){
      latest_event
    } else if (latest_event[,.N] > 1){
      latest_event[order(-`Date of Service Provision`)]
      latest_event[1,]
    } else{
      NULL
    }
  })

  latest_events <- rbindlist(latest_events)
  latest_events <- unique(latest_events)
  latest_events
}

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


has_duplicate_names <- function(events){
  girl_names <- tolower(
    stringr::str_squish(events$`Name of girl`)
    )
  any(duplicated(girl_names))
}


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







