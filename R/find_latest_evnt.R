#' Find latest evnt for follow up
#'
#' Find the latest evnt for follow-up through a systematic search of evnt
#' using the girl ID, name and phone number.
#'
#' @param evnt A data frame with the evnt to search from.
#' @param from Specify a google spreadsheet ID, where to search the evnt.
#' @param ... Additional params passed to the \link[googlesheets4]{read_sheet}.
#' @return the latest unique record, a data.table.
#' @importFrom data.table as.data.table rbindlist
#' @export
find_latest_evnt <- function(evnt, from = NULL, ...){

  if (!is.null(from)){
    sheet_params <- list(...)
    evnt <- googlesheets4::read_sheet(ss = from, col_types = "c", sheet_params)
  }

  evnt <- data.table::as.data.table(evnt)

  evnt <- find_latest_by_name_and_id(evnt)

  evnt <- find_latest_by_phone_number(evnt)

  evnt

}

#' Find latest evnt by girl id and name
#'
#' A systematic search of evnt to identify the latest unique evnt for follow
#' up using the girl ID and name.
#'
#' @param evnt A data.frame or data.table object with the evnt to follow up.
#'
#' @importFrom data.table is.data.table
#' @return the latest unique evnt if present.
find_latest_by_name_and_id <- function(evnt){
  if (!is.data.table(evnt)){
    evnt <- as.data.table(evnt)
  }

  evnt_ls <- lapply(evnt$`Girl ID`, function(x){
    # filter by girl id
    evnt_by_id <- evnt[`Girl ID` == x,]

    if (evnt_by_id[,.N] == 1 && has_phone_number(evnt_by_id)){
      # return the unique and latest
      evnt_by_id
    } else if (evnt_by_id[, .N] > 1 && has_phone_number(evnt_by_id)){
      # check for duplicates names and return latest
      if (has_duplicate_names(evnt_by_id) && has_phone_number(evnt_by_id)){
        # get latest unique evnt
        find_latest_by_name(evnt_by_id)
      } else if (!has_duplicate_names(evnt_by_id) && has_phone_number(evnt_by_id)) {
        # these are unique clients, its by coincidence they have similar names
        evnt_by_id[!is.na(`Phone Number`) & nchar(`Phone Number`) >= 10,]
      } else {
        NULL
      }

    } else {
      NULL
    }

  })

  review_search_result(evnt_ls)

}


#' Find latest evnt by name
#'
#' Identify the latest unique evnt from evnt with duplicated names.
#'
#' @param evnt the duplicated evnt, A data.frame object.
#' @return a list with latest unique evnt
find_latest_by_name <- function(evnt){
  if (!is.data.table(evnt)){
    evnt <- as.data.table(evnt)
  }

  evnt <- evnt[, girl_name := review_names(`Name of girl`)]

  evnt_ls <- lapply(unique(evnt$girl_name), function(x){
    dt <- evnt[girl_name == x,]
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
        key <- dt[!is.na(`KEY`), `KEY`]
        if (length(key) > 1){
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

  review_search_result(evnt_ls)

}

#' Find latest evnt by phone number
#'
#' @param evnt A data.frame or data.table object containing the latest evnt.
#' @return the latest evnt by phone number.
find_latest_by_phone_number <- function(evnt){
  if (!is.data.table(evnt)){
    evnt <- data.table::as.data.table(evnt)
  }
  evnt_ls <- lapply(evnt$`Phone Number`, function(x){
    d <- evnt[`Phone Number` == x]

    if (d[,.N] == 1){
      d
    } else if (d[,.N] > 1){
      d <- d[order(-`Date of Service Provision`)]
      d[1,]
    } else{
      NULL
    }
  })
  review_search_result(evnt_ls)
}

