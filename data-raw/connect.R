# library(a360connect)
# library(googlesheets4)
# library(magrittr)
#
#
# # Historical data ---------------------------------------------------------
#
# testd <- readr::read_csv("/Users/isaiahnyabuto/Documents/Packages/a360connect/tests/testthat/test_data/current_program_a360_full_data.csv",
#                          col_types = paste0(rep("c",43), collapse = ""))
#
# # Find latest
# evnt_to_follow_up <- find_latest_event(testd)
#
# # Transform
# evnt_to_follow_up <- transform_events(evnt_to_follow_up)
#
# # Add to a database
# d <- add_latest_events(evnt_to_follow_up, ssid = "10fvg8YVW7VTLe3PUolUZZO5Ayjtl2TG8IUVRrIb84bs", overwrite = T)
#
#
