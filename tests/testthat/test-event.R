with_mock_api(
  test_that("Pull events works as expected", {
    expect_error(pull_events(baseurl, program_id = "Ymn7R"))
    expect_error(pull_events())
    d <- pull_events(baseurl, program_id = "Ymn7RO0lkf7")
    expect_s3_class(d, class = "psi-mis_api")
    expect_identical(class(d$content$events), "data.frame")
    expect_type(d$endpoint, "character")
    expect_identical(class(d$response), "response")

  })
)

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
