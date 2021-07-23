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

#' Generate a unique girl ID
#'
#' Generate a unique identifier for girls enrolled in the follow up program.
#'
#' \code{generate_uid} generates a random id of size 15 chars.
#' @return A character string of size 15
generate_girl_uid <- function(code_size = 14){
  runif(1)
  allowed_first_chars <- c(LETTERS, 0:9)
  allowed_middle_letters <- LETTERS
  allowed_last_chars <- 0:9
  first_part <- sample(allowed_first_chars, 6)
  middle_part <- sample(allowed_middle_letters, 4)
  last_part <- sample(allowed_last_chars, code_size - 10)
  uid <- paste(
    paste0(first_part, collapse = ""),
    paste0(middle_part, collapse = ""),
    paste0(last_part, collapse = ""),
    sep = "-"
  )
  uid
}

generate_uid <- function(code_size = 11){
  runif(1)
  allowedLetters <- c(LETTERS, letters)
  allowedChars <- c(LETTERS, letters, 0:9)
  firstChar <- sample(allowedLetters, 1)
  otherChars <- sample(allowedChars, codeSize - 1)
  uid <- paste(c(firstChar, paste(otherChars, sep = "", collapse = "")), sep = "", collapse = "")
  uid
}

