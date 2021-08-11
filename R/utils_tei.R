#' Generate TEI payload
#'
#' Create a trackedEntityInstance from a line list
#'
#' @param evnt A list of events from follow up database
#' @return A list, TEI payload
generate_payload <- function(evnt) {
  if (has_KEY(evnt)) {
    evnt <- as.data.table(evnt)
    tei <- data.frame(
      trackedEntityType = "XV3kldsZq0H",
      trackedEntity = evnt$TEI,
      orgUnit = evnt$orgUnit,
      stringsAsFactors = F
    )

    # attributes
    tei$attributes <- lapply(tei$trackedEntity, function(x) {
      dt <- data.table(
        attribute = selected_tea,
        value = clear_names(
          sapply(selected_tea, function(y) {
            evnt[TEI == x, ..y]
          })
        ), stringsAsFactors = F
      )

      # transform dt attributes
      dt[, attribute := plyr::mapvalues(dt$attribute, from = tea_map$name, to = tea_map$id, warn_missing = F)]
    })

    # enrollments
    tei$enrollments <- lapply(tei$trackedEntity, function(x) {
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





clear_names <- function(x) paste0(x)
