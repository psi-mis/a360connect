#' Import Tracked Entity Instances (TEIs) on a DHIS2 server
#'
#' A generic function to upload Tracked Entity Instances on a DHIS2 server.
#'
#' @details [import_teis()] uploads a list of tracked entity instances to a
#'   DHIS2 server. It provides options to specify the import strategy, point to
#'   a DHIS2 instance using the baseurl, and pass authentication. By default,
#'   the basic authentication is supported, and all the teis will be created or
#'   updated where possible.
#'
#'   [create_and_update_teis()] Checks for the existence of a tracked entity
#'   instance on a DHIS2 server before sending a request to create or update the
#'   object. By default, the tracked entity instances are created or updated on
#'   the psi-clone server - this can be changed by supplying the baseurl.
#'
#'   [create_teis()] Generates the payload directly from a data.frame with a
#'   list of tracked entity instances, and posts them to a DHIS2 instance. By
#'   default, the tracked entity instances are posted to the psi-clone server -
#'   this can be changed by supplying the baseurl.
#'
#'   [update_teis()] Generates the payload directly from a data.frame with a
#'   list of existing tracked entity instances, and updates them on a DHIS2
#'   instance. By default, it updates the tracked entity instances to the
#'   psi-clone server - this can be changed by supplying the baseurl.
#'
#'   The baseurl and authentication, if not specified explicitly, will be pulled
#'   from the `r` `environment`.
#'
#' @param teis A data.frame or data.table object containing a list of tracked
#'   entity instances to import.
#' @param strategy A character string specifying the import strategy. Currently
#'   only three options are specified, `create_and_update`, `create` and
#'   `update`. The default is `create_and_update`.
#' @param ... Additional params passed to [create_teis()] and [update_teis()].
#' @return An S3 object with a DHIS2 response, API endpoint and content of the
#'   parsed response.
#' @name import_teis
NULL

#' @describeIn import_teis Imports Tracked Entity Instances (TEIs) on a DHIS2
#'   server
#' @examples
#' \dontrun{
#' import_teis(teis)
#' server_url <- "https://dhis-instance.org/"
#' user <- "admin"
#' pass <- "district"
#' # creates and updates the teis
#' import_teis(teis, baseurl = server_url, user = user, pass = pass)
#' # creates the teis
#' import_teis(teis, strategy = "create", baseurl = server_url, user = user, pass = pass)
#' # updates the teis
#' import_teis(teis, strategy = "update", baseurl = server_url, user = user, pass = pass)
#' }
#' @export
import_teis <- function(teis,
                        strategy = c("create_and_update", "create", "update"),
                        ...) {
  switch(strategy,
    create_and_update = create_and_update_teis(teis, ...),
    create = create_teis(teis, ...),
    update = update_teis(teis, ...)
  )
}

#' @describeIn import_teis Create and Update Tracked Entity Instances (TEIs) on
#'   a DHIS2 server
#'
#' @examples
#' \dontrun{
#' create_and_update_teis(teis)
#' server_url <- "https://dhis-instance.org/"
#' user <- "admin"
#' pass <- "district"
#' create_and_update_teis(teis, baseurl = server_url, user = user, pass = pass)
#' }
#' @export
create_and_update_teis <- function(teis, ...) {
  res <- pb_lapply(teis$trackedEntity, function(x) {
    res1 <- get_tei(x, ...)
    tei <- subset(teis, teis$trackedEntity == x)
    if (http_error(res1)) {
      create_teis(tei, ...)
    } else {
      update_teis(tei, ...)
    }
  })

  res
}



#' @describeIn import_teis Create Tracked Entity Instances (TEIs) on a DHIS2
#'   server
#'
#' @param baseurl The DHIS2 server, default is psi-clone server.
#' @param user DHIS2 User Account, used in the pass key.
#' @param pass Password of the user account, used in the pass key.
#' @examples
#' \dontrun{
#' create_teis(teis)
#' server_url <- "https://dhis-instance.org/"
#' user <- "admin"
#' pass <- "district"
#' create_teis(teis, baseurl = server_url, user = user, pass = pass)
#' }
#' @export
create_teis <- function(teis, baseurl = NULL, user = NULL, pass = NULL) {
  payload <- generate_tei_payload(teis)

  if (is.null(baseurl)) {
    baseurl <- Sys.getenv("BASEURL")
  }

  url <- paste0(baseurl, "api/trackedEntityInstances")
  url <- URLencode(url)

  auth <- check_for_authentication(user, pass)

  httr::POST(url,
    ua, timeout, authenticate(auth$user, auth$pass),
    body = toJSON(
      list(
        trackedEntityInstances = payload
      ),
      auto_unbox = T
    ),
    content_type_json()
  ) -> tei_res

  parse_api_response(tei_res, url)
}

#' @importFrom utils str
print.create_teis <- function(x, ...) {
  cat(sprintf("PSI-MIS <%s>\n", x$endpoint))
  str(x$content, list.len = 5, vec.len = 1)
  invisible(x)
}

#' @describeIn import_teis Update Tracked Entity Instances (TEIs) on a DHIS2
#'   server
#'
#' @param baseurl The DHIS2 server, default is psi-clone server.
#' @param user DHIS2 User Account, used in the pass key.
#' @param pass Password of the user account, used in the pass key.
#' @examples
#' \dontrun{
#' update_teis(teis)
#' server_url <- "https://dhis-instance.org/"
#' user <- "admin"
#' pass <- "district"
#' update_teis(teis, baseurl = server_url, user = user, pass = pass)
#' }
#' @export
update_teis <- function(teis, baseurl = NULL, user = NULL, pass = NULL) {
  payload <- generate_tei_payload(teis)

  if (is.null(baseurl)) {
    baseurl <- Sys.getenv("BASEURL")
  }

  # update
  res <- pb_lapply(payload$trackedEntity, function(x) {
    url <- paste0(baseurl, "api/trackedEntityInstances/", x)
    url <- URLencode(url)

    auth <- check_for_authentication(user, pass)

    httr::PUT(url,
      ua, timeout, authenticate(auth$user, auth$pass),
      body = toJSON(
        list(
          trackedEntityInstances = payload
        ),
        auto_unbox = T
      ),
      content_type_json()
    )
  })

  res <- res[!is_empty(res)]

  d <- pb_lapply(res, function(x) parse_api_response(x, x$url))

  d
}

#' Get Tracked Entity Instance (TEI) from a DHIS2 server
#'
#' @param tei_id The ID of a Tracked Entity Instance to pull.
#' @param baseurl The DHIS2 server, default is psi-clone server.
#' @param user DHIS2 User Account, used in the pass key.
#' @param pass Password of the user account, used in the pass key.
#'
#' @return A DHIS2 response
get_tei <- function(tei_id, baseurl = NULL, user = NULL, pass = NULL) {
  if (is.null(baseurl)) {
    baseurl <- Sys.getenv("BASEURL")
  }

  auth <- check_for_authentication(user, pass)

  res <- httr::GET(
    paste0(baseurl, "api/trackedEntityInstances/", tei_id), ua, timeout,
    authenticate(auth$user, auth$pass)
  )

  res
}

#' Generate TEI payload
#'
#' Create a tracked entity instance payload from a line list
#'
#' @param evnt A list of events to generate a payload.
#' @return A list, TEI payload
generate_tei_payload <- function(evnt) {
  if (has_KEY(evnt)) {
    evnt <- as.data.table(evnt)
    tei <- data.frame(
      trackedEntityType = "XV3kldsZq0H",
      trackedEntity = evnt$TEI,
      orgUnit = evnt$orgUnit,
      stringsAsFactors = F
    )

    # attributes
    tei$attributes <- pb_lapply(tei$trackedEntity, function(x) {
      dt <- data.table(
        attribute = selected_tea,
        value = clear_names(
          sapply(selected_tea, function(y) {
            evnt[TEI == x, ..y]
          })
        ), stringsAsFactors = F
      )

      # transform dt attributes
      dt[, attribute := plyr::mapvalues(dt$attribute,
        from = tea_map$name,
        to = tea_map$id,
        warn_missing = F
      )]
    })

    # enrollments
    tei$enrollments <- pb_lapply(tei$trackedEntity, function(x) {
      dt <- evnt[TEI == x, ]

      if (dt[, !is.na(`Date of follow up call`)]) {
        data.frame(
          orgUnit = dt[, orgUnit],
          program = Sys.getenv("PROGRAM"),
          enrollmentDate = dt[, `Date of follow up call`],
          incidentDate = dt[, `Date of Service Provision`]
        )
      } else {
        NULL
      }
    })

    # filter out the empty teis
    if (any(is_empty(tei$enrollments))){
      warning("skipping teis missing enrollment", call. = F)
      tei <- dplyr::filter(tei, !is_empty(enrollments))
    }

    tei
  }
}

selected_tea <- c(
  "Name of girl", "Girl ID", "Age of Girl",
  "Phone Number", "Method taken up", "Date of Service Provision",
  "Follow-up scheduled (date)", "KEY"
)

tea_map <- data.frame(
  id = c(
    "kn2Zlia7UOw", "JM9qqwDihBV", "zRA08XEYiSF", "SdCcqjOjop1",
    "NEQTN6cdkWu", "oRFL3LnJrRW", "UikwH4obGZJ", "LoGHwYUQZ9y"
  ),
  name = c(
    "Girl ID", "Age of Girl", "KEY",
    "Date of Service Provision", "Method taken up", "Phone Number",
    "Follow-up scheduled (date)", "Name of girl"
  ),
  stringsAsFactors = F
)
