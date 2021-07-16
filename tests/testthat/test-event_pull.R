with_mock_api(
  test_that("Pull events works as expected", {
    expect_error(pull_events(baseurl, program_id = "Ymn7R"))
    d <- pull_events(baseurl, program_id = "Ymn7RO0lkf7")
    expect_s3_class(d, class = "psi-mis_api")
    expect_identical(class(d$content$events), "data.frame")
    expect_type(d$endpoint, "character")
    expect_identical(class(d$response), "response")

  })
)
