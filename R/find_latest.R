#' Identify latest events
#'
#' @param events A dataframe.
#' @return A dataframe of the latest unique record
find_latest_event <- function(events){
  events_dt <- data.table::as.data.table(events)
  events_filtered <- purrr::map(events_dt$`Girl ID`, function(x){

    # filter by girl ID
    events_dt_by_id <- events_dt[`Girl ID` == x]

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
            dt_unique  <- dplyr::select(dt, -`girl_name`)
            dt_unique
          }else if (nrow(dt) > 1 && has_phone_number(dt)){ # if more than on e rwo is returned, overwrite the Phone Numbers
            phone_number <- dt[!is.na(`Phone Number`)]$`Phone Number`
            if (length(phone_number) > 1){
              phone_number <- paste(phone_number, collapse = " | ")
            }
            dt <- dplyr::mutate(dt, `Phone Number` = rep(phone_number, nrow(dt)))
            dt <- dplyr::arrange(dt, desc(as.Date(`Date of Service Provision`)))
            dt <- dplyr::select(dt, -`girl_name`)
            dt_latest <- dt[1,]
            dt_latest
          } else {
            NULL
          }

        })

        events_dt_by_id1

      }else if (!has_duplicate_names(events_dt_by_id) && has_phone_number(events_dt_by_id)){
        # these are unique clients, its just by coincidence they have similar names
        # filter those with phone numbers
        events_dt_by_id[!is.na(`Phone Number`)]
      }else{
        NULL
      }
    }else {
      NULL
    }

  })

  events_filtered
}

has_phone_number <- function(events){
  any(!is.na(events$`Phone Number`) & nchar(events$`Phone Number`) >= 10)
}

has_duplicate_names <- function(events){
  girl_names <- tolower(
    stringr::str_squish(events$`Name of girl`)
    )
  any(duplicated(girl_names))
}
