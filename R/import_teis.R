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
#' @importFrom httr content_type_json
#' @importFrom jsonlite toJSON
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
                        strategy = c("create_and_update",
                                     "create",
                                     "update"),
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
  p <- generate_tei_payload(teis)
  res <- pb_lapply(p$trackedEntityInstance, function(x) {
    res1 <- get_tei(x, ...)
    tei <- subset(teis, teis$TEI == x)

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
#' @param tracked_entity_type DHIS2 ID, the type of tracked entity attribute.
#' @param tea A vector, specifies the tracked attributes.
#' @param tea_mapping A data.frame object containing the tracked entity attributes and their uids.
#' @param is_a360connect_type Logical. Use this to bypass `a360connect` settings.
#' @param is_a360connect_enrollment_type Logical, Use this to bypass `a360connect` settings.
#' @examples
#' \dontrun{
#' create_teis(teis)
#' server_url <- "https://dhis-instance.org/"
#' user <- "admin"
#' pass <- "district"
#' create_teis(teis, baseurl = server_url, user = user, pass = pass)
#' }
#' @export
create_teis <- function(teis,
                        baseurl = NULL,
                        user = NULL,
                        pass = NULL,
                        tracked_entity_type = NULL,
                        tea = NULL,
                        tea_mapping = NULL,
                        is_a360connect_type = TRUE,
                        is_a360connect_enrollment_type = TRUE) {
  output_progress("Creating tracked entity instances",
                  cli_fun = "cli_h2")

  payload <- generate_tei_payload(teis)

  if (is.null(baseurl)) {
    baseurl <- Sys.getenv("BASEURL")
  }

  url <- paste0(baseurl, "api/trackedEntityInstances?importStrategy=CREATE")
  url <- URLencode(url)

  auth <- check_for_authentication(user, pass)

  output_progress("Creating tracked entity instances",
                  cli_fun = "cli_alert_info")

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

  output_progress("Created tracked entity instances successively",
                  cli_fun = "cli_alert_success",
                  crayon_fun = "green")

  output_progress("parsing api response",
                  cli_fun = "cli_alert_info")

  d <- parse_api_response(tei_res, url)

  output_progress("parsed api response successively",
                  cli_fun = "cli_alert_success",
                  crayon_fun = "green")
  d
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
#' @param tracked_entity_type DHIS2 ID, the type of tracked entity attribute.
#' @param tea A vector, specifies the tracked attributes.
#' @param tea_mapping A data.frame object containing the tracked entity attributes and their uids.
#' @param is_a360connect_type Logical. Use this to bypass `a360connect` settings.
#' @param is_a360connect_enrollment_type Logical, Use this to bypass `a360connect` settings.
#' @examples
#' \dontrun{
#' update_teis(teis)
#' server_url <- "https://dhis-instance.org/"
#' user <- "admin"
#' pass <- "district"
#' update_teis(teis, baseurl = server_url, user = user, pass = pass)
#' }
#' @export
update_teis <- function(teis,
                        baseurl = NULL,
                        user = NULL,
                        pass = NULL,
                        tracked_entity_type = NULL,
                        tea = NULL,
                        tea_mapping = NULL,
                        is_a360connect_type = TRUE,
                        is_a360connect_enrollment_type = TRUE) {
  output_progress("updating tracked entity instances",
                  cli_fun = "cli_h2")

  payload <- generate_tei_payload(teis)

  if (is.null(baseurl)) {
    baseurl <- Sys.getenv("BASEURL")
  }

  # update
  output_progress("updating tracked entity instances",
                  cli_fun = "cli_alert_info")
  res <- pb_lapply(payload$trackedEntityInstance, function(x) {
    url <- paste0(baseurl, "api/trackedEntityInstances/", x, "?importStrategy=UPDATE&ignoreEmptyCollection=true")
    url <- URLencode(url)

    tei <- payload[payload$trackedEntityInstance == x,]

    auth <- check_for_authentication(user, pass)

    httr::PUT(url,
      ua, timeout, authenticate(auth$user, auth$pass),
      body = toJSON(
        parse_tei_payload(tei),
        auto_unbox = T
      ),
      content_type_json()
    )
  })

  output_progress("updated tracked entity instances successively",
                  cli_fun = "cli_alert_success",
                  crayon_fun = "green")

  output_progress("parsing api responses",
                  cli_fun = "cli_alert_info")

  res <- res[!is_empty(res)]

  d <- pb_lapply(res, function(x) parse_api_response(x, x$url))

  output_progress("parsed api responses successively",
                  cli_fun = "cli_alert_success",
                  crayon_fun = "green")

  d
}

#' Get Tracked Entity Instance (TEI) from a DHIS2 server
#'
#' @param tei_id The ID of a Tracked Entity Instance to pull.
#' @param baseurl The DHIS2 server, default is psi-clone server.
#' @param user DHIS2 User Account, used in the pass key.
#' @param pass Password of the user account, used in the pass key.
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
#' @importFrom data.table data.table as.data.table
#'
#' @param evnt A data.frame of events to generate a payload.
#' @param tracked_entity_type DHIS2 ID, the type of tracked entity attribute.
#' @param tea A vector, specifies the tracked attributes.
#' @param tea_mapping A data.frame object containing the tracked entity attributes and their uids.
#' @param is_a360connect_type Logical. Use this to bypass `a360connect` settings.
#' @param is_a360connect_enrollment_type Logical, Use this to bypass `a360connect` settings.
#' @return A list, TEI payload
generate_tei_payload <- function(evnt,
                                 tracked_entity_type = NULL,
                                 tea = NULL,
                                 tea_mapping = NULL,
                                 is_a360connect_type = TRUE,
                                 is_a360connect_enrollment_type = TRUE) {
  output_progress("generating tei payload",
                  cli_fun = "cli_h3")

  if (!is.data.frame(evnt)){
    stop("evnt must be a data.frame object", call. = F)
  } else {
    # make the event a data.table, we will use this to easily filter by columns
    # & more efficiently compared to a df object.
    evnt <- as.data.table(evnt)
  }

  if (is_a360connect_type){
    if (!has_KEY(evnt)){
      stop("evnt must have a special column,
           `KEY`, that uniquely identifys the events to generate TEIs", call. = F)
    }
  }

  tei <- data.frame(
    trackedEntityType = ifelse(!is.null(tracked_entity_type),
                               tracked_entity_type,
                               Sys.getenv("TRACKED_ENTITY_TYPE")),
    orgUnit = evnt$orgUnit,
    stringsAsFactors = F
  )

  if (!has_tei_ids(evnt)){
    stop("evnt must have a special column, `TEI`, with the TEI ids")
  } else {
    tei$trackedEntityInstance <- evnt$TEI
  }

  # set attributes
  if (!is.null(tea) && !is.null(tea_mapping)){
    tei$attributes <- lapply(tei$trackedEntityInstance, function(x) {
      dt <- data.table(
        attribute = tea,
        value = clear_names(
          sapply(tea, function(y) {
            evnt[TEI == x, ..y]
          })
        ), stringsAsFactors = F
      )

      # transform dt attributes
      dt[, attribute := plyr::mapvalues(dt$attribute,
                                        from = tea_mapping$name,
                                        to = tea_mapping$id,
                                        warn_missing = F
      )]
    })
  } else {
    tei$attributes <- lapply(tei$trackedEntityInstance, function(x) {
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
  }

  output_progress("setting tracked entity attributes",
                  cli_fun = "cli_alert_info")

  # set enrollments
  if (!is_a360connect_enrollment_type) {
    # for generic use case
    tei$enrollments <- lapply(tei$trackedEntityInstance, function(x) {
      dt <- evnt[TEI == x, ]

      enrollment <- data.frame(
        orgUnit = dt[, orgUnit],
        program = Sys.getenv("PROGRAM"),
        stringsAsFactors = F
      )

      if (has_incident_date(dt)) {
        enrollment$incidentDate <- dt[, `incident_date`]
      }

      if (has_enrollment_date(dt)) {
        enrollment$enrollmentDate <- dt[, `enrollment_date`]
      }

      enrollment

    })

  } else {
    # default to A360 connect use case
    tei$enrollments <- lapply(tei$trackedEntityInstance, function(x) {
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
  }

  output_progress("attaching enrollments",
                  cli_fun = "cli_alert_info")

  # filter out the empty teis
  if (any(is_empty(tei$enrollments))) {
    if (interactive()){
      warning("skipping teis missing an enrollment", call. = F)
    }
    tei <- dplyr::filter(tei, !is_empty(enrollments))
  }

  output_progress("payload generated successively",
                  cli_fun = "cli_alert_success",
                  crayon_fun = "green")

  tei
}

#' Selected Tracked Entity Attributes
#'
#' A vector containing a names of tracked entity attributes implemented in `A360`
#' `provider Call Log Program`
#'
#' @format
#' \describe{
#'   \item{Name of girl}{name of a tracked entity attribute, captures the name of girl}
#'   \item{Girl ID}{name of a tracked entity attribute, captures the A360 girl id}
#'   \item{Phone Number}{name of a tracked entity attribute, captures the Phone Number}
#'   \item{Method taken up}{name of a tracked entity attribute, records the method taken up}
#'   \item{Date of Service Provision}{name of a tracked entity attribute, records the date of service provision}
#'   \item{Follow-up scheduled (date)}{name of a tracked entity attribute, captures the scheduled date of the follow up call}
#'   \item{KEY}{refers to the Unique Identifier for the TEIs, captured in `CORE - Unique ID (UIC) `}
#' }
selected_tea <- c(
  "Name of girl", "Girl ID", "Age of Girl",
  "Phone Number", "Method taken up", "Date of Service Provision",
  "Follow-up scheduled (date)", "KEY"
)

#' Tracked Entity Attributes data map
#'
#' A dataset containing the ids and name tracked entity attributes used in `A360`
#' `Provider Call Log program`
#'
#' @format
#' \describe{
#'   \item{id}{id, Unique ID of the tracked entity attribute}
#'   \item{name}{name,name of the tracked entity attribute}
#' }
#' @source \url{https://staging.psi-mis.org/}
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

parse_tei_payload <- function(tei){

  if (is_tei_payload(tei)) {
    list(
      trackedEntityType = tei$trackedEntityType[1],
      trackedEntityInstance = tei$trackedEntityInstance[1],
      orgUnit = tei$orgUnit[1],
      attributes = tei$attributes[[1]],
      enrollments = tei$enrollments[[1]]
    )
  }

}
