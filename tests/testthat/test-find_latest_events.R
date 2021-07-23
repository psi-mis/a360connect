# testd <- readxl::read_xlsx("./test_data/a360 test - July 2021.xlsx", sheet = "current_program_a360_full_data",
#                            col_types = "text")
#
#
# test_that("has_phone_number checks for presence of any phone number",{
#   d2 <- as.data.table(testd)
#   d2 <- d2[is.na(`Phone Number`),]
#   expect_equal(has_phone_number(testd), T)
#   expect_equal(has_phone_number(d2), F)
# })
#
#
# test_that("find latest events returns a data.table", {
#   expect_identical(class(find_latest_event(testd)), "data.table")
# })
