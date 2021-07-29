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

test_that("has_phone_number checks for presence of any phone number",{
  d2 <- as.data.table(testd)
  d2 <- d2[is.na(`Phone Number`),]
  expect_equal(has_phone_number(testd), T)
  expect_equal(has_phone_number(d2), F)
})


test_that("Add latest events works as expected",{
  d_init <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  # update
  d <- add_latest_events(testd, sheet = "Sheet1", ssid = "1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  d_after <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  # overwrite
  d2_init <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  d2 <- add_latest_events(testd, sheet = "Sheet1", ssid = "1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA", overwrite = T)
  d2_after <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  expect_equal(add_latest_events(), NULL)
  expect_equal(d_init$sheets$grid_rows < d_after$sheets$grid_rows, T)
  expect_equal(d2_init$sheets$grid_rows > d2_after$sheets$grid_rows, T)
})
