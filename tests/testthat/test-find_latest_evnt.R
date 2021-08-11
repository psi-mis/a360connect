testd <- readr::read_csv("./test_data/current_program_a360_full_data.csv",
  col_types = paste0(rep("c", 43), collapse = "")
)

testd <- testd[1:1000, ]

test_that("Find latest evnt returns unique evnt", {
  d <- find_latest(testd)
  # girl_names <- stringr::str_squish(d$`Name of girl`)
  # girl_names <- stringr::str_to_lower(girl_names)
  # girl_names <- stringr::str_trim(girl_names)
  expect_equal(d[, .N], data.table::uniqueN(d, by = "Phone Number"))
})


test_that("Find latest by name identifies the latest unique evnt from a df of evnt with duplicated names", {
  # test for no duplicates
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x, ])
  d1 <- find_latest_by_name(d[[415]])
  expect_equal(d1$`Phone Number`, "7080401100")

  d2 <- find_latest_by_name_and_id(d[[415]])
  expect_equal(d1, d2)
})


test_that("Find latest by name maintains the phone number for duplicated data", {
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x, ])
  d1 <- d[[415]]
  d1$`Phone Number` <- c("7080401100", "8080401100", NA, NA)
  d1$`Date of Service Provision` <- c("2019-06-17", "2019-06-18", "2019-06-19", "2019-06-28")
  d1 <- find_latest_by_name(d1[-1, ])
  expect_equal(d1$`Date of Service Provision`, "2019-06-28")
})


test_that("Find latest by name maintains the KEY for duplicated data", {
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x, ])
  d <- d[[415]]
  d$`Phone Number` <- c("7080401100", "8080401100", NA, NA)
  d$`Date of Service Provision` <- c("2019-06-17", "2019-06-18", "2019-06-19", "2019-06-28")
  # test for data with keys
  d$`KEY` <- c(NA, NA, "3Y2ORG-GWVN-5709", NA)
  d <- find_latest_by_name(d[-1, ])

  expect_equal(d$`KEY`, "3Y2ORG-GWVN-5709")
})

test_that("Find latest by name returns an error when multiple KEYS are found", {
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x, ])
  d <- d[[415]]
  d$`Phone Number` <- c("7080401100", "8080401100", NA, NA)
  d$`Date of Service Provision` <- c("2019-06-17", "2019-06-18", "2019-06-19", "2019-06-28")
  # test for data with keys
  d$`KEY` <- c(NA, NA, "3Y2ORG-GWVN-5709", "I1D7K8-NJBA-9063")
  expect_error(find_latest_by_name(d[-1, ]), regexp = "Multiple KEYS found!")
})

test_that("Find latest by name returns NULL when no follow up data is found", {
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x, ])
  d <- d[[415]]
  d <- find_latest_by_name(d[-1, ])
  expect_equal(d, NULL)
})


# Test latest evnt
test_that("Find latest by name and id checks for non duplicates having a phone number", {
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x, ])
  d <- d[[415]]
  d$`Phone Number` <- c("7080401100", "8080401100", NA, NA)
  d$`Name of girl` <- paste0(d$`Name of girl`, 1:4)
  d <- find_latest_by_name_and_id(d)
  expect_equal(d$`Phone Number`, c("7080401100", "8080401100"))
})

test_that("Find latest by name and id returns NULL if the evnt are missing a phone number", {
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x, ])
  d <- d[[415]]
  d$`Phone Number` <- c(NA, NA, NA, NA)
  d$`Name of girl` <- paste0(d$`Name of girl`, 1:4)
  d <- find_latest_by_name_and_id(d)
  expect_equal(d, NULL)
})


# Test latest evnt by phone number
test_that("Find latest by phone number returns the latest unique record by phone number", {
  d <- lapply(testd$`Girl ID`, function(x) testd[testd$`Girl ID` == x, ])
  d <- d[[415]]
  d$`Phone Number` <- c("8080401100", "8080401100", NA, NA)
  d$`Date of Service Provision` <- c("2019-06-17", "2019-06-18", "2019-06-19", "2019-06-28")
  d <- find_latest_by_phone_number(d)
  expect_equal(d$`Date of Service Provision`, "2019-06-18")
})
