testd <- readr::read_csv("./test_data/current_program_a360_full_data.csv",
  col_types = paste0(rep("c", 43), collapse = "")
)

testd <- testd[1:1000, ]

test_that("Add latest evnt works as expected", {
  d_init <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  # update
  d <- add_latest_evnt(testd, sheet = "Sheet1", ssid = "1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  d_after <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  # overwrite
  d2_init <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  d2 <- add_latest_evnt(testd, sheet = "Sheet1", ssid = "1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA", overwrite = T)
  d2_after <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  expect_equal(add_latest_evnt(), NULL)
  expect_equal(d_init$sheets$grid_rows < d_after$sheets$grid_rows, T)
  expect_equal(d2_init$sheets$grid_rows > d2_after$sheets$grid_rows, T)
})
