library(a360connect)
library(googlesheets4)
library(magrittr)
library(gsheetr)

baseurl <- "https://staging.psi-mis.org/"
base <- substr(baseurl, 9, 27)

gsheetr::login_dhis2(baseurl,
  usr = keyringr::get_kc_account(base, "internet"),
  pwd = keyringr::decrypt_kc_pw(base, "internet")
)



# Historical data ---------------------------------------------------------

# testd <- readr::read_csv("/Users/isaiahnyabuto/Documents/Packages/a360connect/tests/testthat/test_data/current_program_a360_full_data.csv",
#                         col_types = paste0(rep("c",43), collapse = ""))

# Find latest
# evnt_to_follow_up <- find_latest_event(testd)

# Transform
# evnt_to_follow_up <- transform_events(evnt_to_follow_up)

# Add to a database
# d <- add_latest_events(evnt_to_follow_up, ssid = "10fvg8YVW7VTLe3PUolUZZO5Ayjtl2TG8IUVRrIb84bs", overwrite = T)


# update current data -----------------------------------------------------
# d <- googlesheets4::read_sheet(ss = "10fvg8YVW7VTLe3PUolUZZO5Ayjtl2TG8IUVRrIb84bs", sheet = "latest_events")

# # Remove KEY
# d <- dplyr::select(d, -KEY)

# overwrite to the database
# d <- add_latest_evnt(d, sheet = "latest_events", ssid = "10fvg8YVW7VTLe3PUolUZZO5Ayjtl2TG8IUVRrIb84bs", overwrite = T)


# Create TEIs -------------------------------------------------------------

# get the TEIs
teis <- googlesheets4::read_sheet(ss = "10fvg8YVW7VTLe3PUolUZZO5Ayjtl2TG8IUVRrIb84bs", sheet = "latest_events")

# remove invalid references (ou conflicts)
teis <- dplyr::filter(teis, !orgUnit %in% c("yJYP26YnyZM", "ASJcs9yrY3x"))

# Update the Program OUS
program_d <- httr::GET(
  paste0(baseurl, "api/29/programs/QXuYEgGmTjX.json")
) %>%
  httr::content(., "text") %>%
  jsonlite::fromJSON(.)

# update ous
program_d$organisationUnits <- data.frame(
  id = unique(teis$orgUnit),
  stringsAsFactors = F
)

httr::PUT(
  paste0(baseurl, "api/29/programs/QXuYEgGmTjX"),
  body = toJSON(
    program_d,
    auto_unbox = T
  ),
  content_type_json()
) -> d


##' Map the Methods to option codes
# opts_map <- httr::GET(paste0(baseurl, "api/29/optionSets/MujeDcPigJK.json?fields=options[id,name,code]")) %>%
#   httr::content(., "text") %>%
#   fromJSON(.) %>%
#   .$options
#
# teis$`Method taken up` <- plyr::mapvalues(teis$`Method taken up`,
#   from = opts_map$name,
#   to = opts_map$code, warn_missing = F
# )

# update the new mappings
# d <- a360connect::add_latest_evnt(teis,
#                                   ssid = "10fvg8YVW7VTLe3PUolUZZO5Ayjtl2TG8IUVRrIb84bs",
#                                   sheet = "latest_events",
#                                   overwrite = T)

## Filter those on short term
teis <- dplyr::filter(teis, `Method taken up` %in% c(
  "INJ_Norigynon", "INJ_Nor", "INJ_Depo",
  "INJ_Sayana", "INJ_Sayana_Self", "OC_Micro",
  "OC_Combi3", "OC_Escluton", "CycleBeads",
  "Condoms", "FemaleCondoms", "EC"
))
## Filter starting April

teis_april <- dplyr::filter(teis, teis$`Date of Service Provision` > "2021-04-01")

## Filter those without Date of follow up
teis_april <- dplyr::filter(teis_april, !is.na(`Date of follow up call`))

# Create one TEIs
# d <- vector("list", length = )
# for ()
teis_split <- split(teis_april, rep(1:nrow(teis_april) / 50, each = 50)[1:nrow(teis_april)])

d <- vector("list", length(teis_split))

for (i in seq_along(1:length(teis_split))) {
  d[[i]] <- a360connect::create_teis(baseurl, teis = teis_split[[i]])
}


# Update TEI uids on the google spreadhseet -------------------------------

tei_pages <- pbapply::pblapply(1:129, function(x) {
  paste0(
    baseurl,
    "api/29/trackedEntityInstances.json?ou=Mer51uyEWNI&ouMode=DESCENDANTS&fields=*&includeAllAttributes=True&program=QXuYEgGmTjX&pageSize=1000",
    "&page=", x
  )
})

all_teis <- httr::GET(
  paste0(
    baseurl,
    "api/29/trackedEntityInstances.json?ou=Mer51uyEWNI&ouMode=DESCENDANTS&fields=*&includeAllAttributes=True&program=QXuYEgGmTjX&paging=false"
  )
)

all_teis <- fromJSON(content(all_teis, "text"))

tei_attr <- pbapply::pblapply(all_teis$trackedEntityInstances$trackedEntityInstance, function(x) {
  tei <- all_teis$trackedEntityInstances[all_teis$trackedEntityInstances$trackedEntityInstance == x, ]
  tei <- tidyr::pivot_wider(tei$attributes[[1]], names_from = displayName, values_from = value)
  tei <- dplyr::filter(tei, !is.na(`CORE - Unique ID (UIC)`))
  tei <- dplyr::mutate(tei, id = x)
  dplyr::select(tei, c(id, `CORE - Unique ID (UIC)`))
})

tei_attr <- dplyr::bind_rows(tei_attr)
