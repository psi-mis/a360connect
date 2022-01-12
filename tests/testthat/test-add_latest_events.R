testd <- readr::read_csv("./test_data/current_program_a360_full_data.csv",
  col_types = paste0(rep("c", 43), collapse = "")
)

testd <- testd[1:1000, ]

test_that("Add latest events works as expected", {
  d_init <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  # update
  d <- add_latest_events(testd, sheet = "Sheet1", ssid = "1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  d_after <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  # overwrite
  d2_init <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  d2 <- add_latest_events(testd, sheet = "Sheet1", ssid = "1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA", overwrite = T)
  d2_after <- googlesheets4::gs4_get("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA")
  expect_equal(add_latest_events(), NULL)
  expect_equal(d_init$sheets$grid_rows[1] < d_after$sheets$grid_rows[1], T)
  expect_equal(d2_init$sheets$grid_rows[1] > d2_after$sheets$grid_rows[1], T)
})

test_that("Girl UID is generated correctly", {
  girl_id <- generate_girl_uid()
  expect_equal(nchar(girl_id), 16)
  part <- strsplit(girl_id, split = "-")
  expect_equal(nchar(part[[1]][1]),6)
  expect_equal(nchar(part[[1]][2]),4)
  expect_equal(nchar(part[[1]][3]),4)
})

test_that("DHIS2 Object UID is generated correctly", {
  id <- generate_uid()
  expect_equal(nchar(id), 11)
  part <-strsplit(id,"")
  expect_equal(part[[1]][1] %in% c(letters,LETTERS),TRUE)
  expect_equal(all(part[[1]][2:11] %in% c(letters, LETTERS, 0:9)),TRUE)
})


test_that("Random file code is generated correctly", {
  id <- generate_random_code()
  expect_equal(nchar(id), 19)
  part <-strsplit(id,"-")
  expect_equal(nchar(part[[1]][3]),4)
})
