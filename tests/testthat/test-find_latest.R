testd <- readr::read_csv("./test_data/current_program_a360_full_data.csv",
                           col_types = paste0(rep("c",43), collapse = ""))

testd <- testd[1:1000,]

test_that("Find latest events returns unique event", {
  d <- find_latest_event(testd)
  # girl_names <- stringr::str_squish(d$`Name of girl`)
  # girl_names <- stringr::str_to_lower(girl_names)
  # girl_names <- stringr::str_trim(girl_names)
  expect_equal(d[, .N], data.table::uniqueN(d, by = "Phone Number"))
})


test_that("Find latest unique event identifies the latest unique event from a df of events with duplicated names",{
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x,])
  d1 <- find_latest_unique_event(d[[415]])
  d1 <- data.table::rbindlist(d1)
  expect_equal(d1$`Phone Number`, "7080401100")

  d2 <- d[[415]]
  d2$`Phone Number` <- c("7080401100","8080401100", NA, NA)
  d2$`Date of Service Provision` <- c("2019-06-17", "2019-06-18", "2019-06-19", "2019-06-28")
  d3 <- find_latest_unique_event(d2[-1,])
  d3 <- rbindlist(d3)
  expect_equal(d3$`Date of Service Provision`, "2019-06-28")
})


