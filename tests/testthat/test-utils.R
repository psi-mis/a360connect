
d <- data.table(name = c("Isaiah    Nyabuto",
                         "Isaiah Nyabuto  ",
                         "   Isaiah Nyabuto"),
                id = rep(123, 3))

test_that("names are correctly reviewed", {
  d <- d[, name := review_names(name)]
  expect_equal(d$name, rep("isaiah nyabuto",3))
})

testd <- readr::read_csv("./test_data/current_program_a360_full_data.csv",
                         col_types = paste0(rep("c",43), collapse = ""))
testd <- testd[1:1000,]

test_that("has_phone_number checks for presence of any phone number",{
  d2 <- as.data.table(testd)
  d2 <- d2[is.na(`Phone Number`),]
  expect_equal(has_phone_number(testd), T)
  expect_equal(has_phone_number(d2), F)
})
