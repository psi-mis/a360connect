testd <- readr::read_csv("./test_data/current_program_a360_full_data.csv",
                         col_types = paste0(rep("c",43), collapse = ""))
testd <- testd[1:1000,]

test_that("Events are correctly transformed",{
  d <- jsonlite::read_json("./clone.psi-mis.org/api/events-1cc8f8.json", simplifyVector = T)
  d <- transform_events(d$events)
  d2 <- transform_events(testd) # test - find latest
  expect_equal(names(d),required_fields)
  expect_equal(names(d), required_fields)
})
