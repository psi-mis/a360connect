library(magrittr)
library(ggplot2)
library(httr)

des <- httr::GET(
  paste0(Sys.getenv("CLONE"), "api/programStages/UNcBM7ZAXsO?fields=programStageDataElements[dataElement[id,name]]"),
  authenticate(Sys.getenv("C_USER"), Sys.getenv("C_PASS"))
) %>%
  httr::content(., "text") %>%
  jsonlite::fromJSON(.) %>%
  .$programStageDataElements %>%
  .$dataElement

short_name <- strsplit(des$name, " - ")
short_name <- rapply(short_name, function(x) tail(x, 1)) %>%
  trimws(., "both")
des$name <- short_name


# usethis::use_data(des, internal = T, overwrite = T)

# ng_prov_cl_metadata <- GET(paste0(devurl, 'api/programs/QXuYEgGmTjX/metadata.json')) %>%
#   content(., 'text') %>%
#   fromJSON(.)
#
# tea_map <- ng_prov_cl_metadata %>%
#   .$trackedEntityAttributes %>%
#   dplyr::select(name, id)

# tea_map <- readr::read_csv("/Users/isaiahnyabuto/Documents/Workspace/A360/NG A360 call center/prov-cl-tea.csv",
#                            col_types = paste0(rep("c",3), collapse = ""))

usethis::use_data(tea_map, internal = F)


a360ngdata <- readr::read_csv("/Users/isaiahnyabuto/Documents/Workspace/A360/NG Data/current_program_a360_full_data.csv",
  col_types = paste0(rep("c", 43), collapse = "")
)


a360ngdata %>% names(.)



# What are the duplicates? ----------------------------------------------------
# ggplot(a360ngdata, mapping = aes(x = as.Date(Date), fill = `Duplicated`))  +
#   geom_dotplot(binwidth = 50)





# Are there any NAs in the filed: New registered client? ------------------

#' Yes. There are 23 records with NAs, 3 reported in 2020 and 20 in 2019.
#' the last NA was reported in 2020-03-12.
#'
a360ngdata %>%
  dplyr::filter(., is.na(a360ngdata$"Newly registered client")) %>%
  dplyr::arrange(., desc(Date)) %>%
  ggplot(., mapping = aes(as.Date(Date))) -> p

p + geom_histogram(stat = "count", position = "identity") +
  xlab("Period") +
  labs(
    title = "Can we depend on the newly registered client field to merge the duplicates",
    subtitle = "Analysis of empty fields (NAs) 2019 - 2020", caption = "A360 NG | DHIS2"
  )



# Are there any NAs in thee field: type of visit? -------------------------
a360ngdata %>%
  dplyr::filter(., is.na(a360ngdata$`Visit Type (First/Follow-up/Repeat)`)) %>%
  dplyr::arrange(., desc(Date)) %>%
  ggplot2::ggplot(., mapping = aes(as.Date(Date))) -> p

p + geom_histogram(stat = "count", position = "identity") +
  xlab("Period") +
  labs(
    title = "Can we depend on the visit type to merge the duplicates?",
    subtitle = "Analysis of empty fields (NAs) 2019-01 - 2021-06", caption = "A360 NG | DHIS2"
  )



# ? ------------------------------------

# convert nA360ngdata to data bele
a360ngdata2 <- data.table::as.data.table(a360ngdata)

purrr::map(a360ngdata$`Girl ID`, ~ a360ngdata2[`Girl ID` == .x]) -> d3

# I realize only 22 % of clients have a close to a valid phone number
a360ngdata2[(!is.na(`Phone Number`) & nchar(`Phone Number`) >= 10)] %>% nrow(.)

## Filter clients by Girl ID,
#' -> Does the any of the records have a phone number?
#' > Identify unique clients;
#' ----- is unique by name of the girl & has a phone number  ----> store a side
#' --- If names are duplicate duplicates
#' ----------> Filter where name is duplicated
#' ---------------> check if any has a phone number, if not ignore.
#' ----------------------> if phone number is present, overwrite in the column & sort by date of service provision
#' -----------------------------> return the latest
#' filter repeat users
a360ngdata_dt <- data.table::as.data.table(a360ngdata)

a360ngdata_dt[!is.na(`Phone Number`) & nchar(`Phone Number`) == 100, .N > 0]
