ua <- httr::user_agent("https://github.com/psi-mis/a360connect")
timeout <- httr::timeout(600)

#' @importFrom httr http_type http_error content status_code
#' @importFrom jsonlite fromJSON
check_response <- function(res = NULL){
  if (!is.null(res)){
    if (http_type(res) != "application/json"){
      stop("psi-mis did not return json", call. = F)
    }
    parsed <- fromJSON(content(res, "text"), simplifyVector = F)
    if (http_error(res)){
      stop(
        sprintf("psi-mis request failed [%s] \n%s\n<%s>",
                status_code(res),
                parsed$message,
                parsed$url), call. = F
      )
    }
  }
}



