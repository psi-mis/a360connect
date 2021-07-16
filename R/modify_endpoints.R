#' Modify Event Endpoint
#'
#' Code to modify Event API Endpoint
#' @inheritParams pull_events
#' @return url
#' @importFrom utils URLencode
modify_events_endpoint <- function(baseurl = NULL,program_id = NULL,
                                   startDate = NULL, endDate = NULL,
                                   paging = TRUE, pageSize = 50){
  if (!is.null(baseurl) && !is.null(program_id)){
    if (!paging){
      url <- paste0(baseurl, "api/events?paging=false&program=", program_id)
    }else{
      url <- paste0(baseurl, "api/events?paging=true&totalPages=true&program=", program_id, "&pageSize=", pageSize)
    }

    if (!is.null(startDate)){
      url <- paste0(url,"&startDate=", startDate)
    }
    if (!is.null(endDate)){
      url <- paste0(url,"&endDate=", endDate)
    }
    URLencode(url)
  }else{
    stop("a360connect: atleast both the baseurl and the program id must be specified", call. = F)
  }
}

