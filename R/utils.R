ua <- httr::user_agent("https://github.com/psi-mis/a360connect")
timeout <- httr::timeout(60)

#' @importFrom httr http_type http_error content status_code
#' @importFrom jsonlite fromJSON
check_for_response <- function(res = NULL){
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

check_for_authentication <- function(user = NULL, pass = NULL){
  if (is.null(user) && is.null(pass)){
    user <- Sys.getenv("C_USER")
    pass <- Sys.getenv("C_PASS")

    if (user == "" || pass == ""){
      stop("a360connect: no authenication key found, please specify")
    }
  }
  list(user = user, pass = pass)
}


