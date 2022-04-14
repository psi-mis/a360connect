library(httptest)
library(httr)

baseurl <- Sys.getenv("CLONE")
user <- Sys.getenv("C_USER")
pass <- Sys.getenv("C_PASS")

# start_capturing()
# get_events(baseurl = baseurl, program_id = "Ymn7RO0lkf7")
# stop_capturing()
#
# # get tei schema
#
# start_capturing()
#
# res <- httr::GET(paste0(baseurl, "api/schemas/trackedEntityInstance.json"), authenticate(user,pass))
#
# httr::content(res, "text")
#
# stop_capturing()
