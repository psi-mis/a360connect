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

#' Search
#'
#' @importFrom data.table is.data.table
latest_events <- function(evts, has_key = F){
  if (!is.data.table(evts)){
    evts <- as.data.table(evts)
  }

  evt_ls <- lapply(evts$`Girl ID`, function(x){
    # filter by girl id
    evts_by_id <- evts[`Girl ID` == x,]

    if (evts_by_id[,.N] == 1 && has_phone_number(evts_by_id)){
      # return the unique and latest
      evts_by_id
    } else if (evts_by_id[, .N] > 1 && has_phone_number(evts_by_id)){
      # check for duplicates names and return latest
      if (has_duplicate_names(evts_by_id) && has_phone_number(evts_by_id)){
        # get latest unique events
        find_latest_unique_event(evts_by_id)
      } else if (!has_duplicate_names(evts_by_id) && has_phone_number()) {
        # these are unique clients, its by coincidence they have similar names
        evts_by_id[!is.na(`Phone Number`) & nchar(`Phone Number`) >= 10,]
      } else {
        NULL
      }

    } else {
      NULL
    }

  })

}


#' Identify latest unique events for follow up
#'
#' Identify latest unique events from events with duplicated names
#' @param evts the duplicated events, A data.frame object.
#' @return a list with latest unique event
find_latest_unique_event <- function(evts){
  if (!is.data.table(evts)){
    evts <- as.data.table(evts)
  }

  evts <- evts[, girl_name := review_names(`Name of girl`)]

  evts_ls <- lapply(unique(evts$girl_name), function(x){
    dt <- evts[girl_name == x,]
    if (dt[, .N] == 1 && has_phone_number(dt)){
      dt[, -"girl_name"]
    } else if (dt[, .N] > 1 && has_phone_number(dt)){
      # review and overwrite the phone numbers
      phone_number <- dt[!is.na(`Phone Number`) & nchar(`Phone Number`) >= 10, `Phone Number`]
      if (length(phone_number) > 1){
        phone_number <- paste(phone_number, collapse = " | ")
      }

      dt <- dt[,`Phone Number`:= rep(phone_number, .N)]

      # copy keys
      if (has_KEY(dt)){
        KEY <- dt[is.na(`KEY`), `KEY`]
        if (length(KEY) > 1){
          stop(sprintf(
            "Multiple KEYS found! \n
            <%s>", paste(KEY, collapse = "; ")
          ), call. = F)
        }
        # overwrite key
        dt <- dt[, KEY := rep(KEY, .N)]
      }

      # sort the Date of Service Provision in descending order
      dt <- dt[order(-`Date of Service Provision`)]
      # latest
      dt[1, -"girl_name"]
    } else {
      NULL
    }
  })

  evts_ls
}







