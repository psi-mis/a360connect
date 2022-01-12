with_mock_api(
  test_that("get events works as expected", {
    expect_error(get_events(baseurl, program_id = "Ymn7R"))
    expect_error(get_events())
    d <- get_events(baseurl, program_id = "Ymn7RO0lkf7")
    expect_s3_class(d, class = "psi-mis_api")
    expect_identical(class(d$content$events), "data.frame")
    expect_type(d$endpoint, "character")
    expect_identical(class(d$response), "response")
    expect_equal(d$response$status, 200L)
  })
)




test_that("events endpoint correctly generates the url", {
  expect_error(modify_events_endpoint())
  expect_error(modify_events_endpoint(startDate = "2018-01-01", ))
  expect_error(modify_events_endpoint(baseurl = baseurl))
  expect_equal(
    modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7"),
    "https://clone.psi-mis.org/api/events.json?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50"
  )
  expect_equal(
    modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", paging = F),
    "https://clone.psi-mis.org/api/events.json?paging=false&program=Ymn7RO0lkf7"
  )
  expect_equal(
    modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", startDate = "2018-01-01"),
    paste0(baseurl, "api/events.json?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50&startDate=2018-01-01")
  )
  expect_equal(
    modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", endDate = "2018-01-01"),
    paste0(baseurl, "api/events.json?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50&endDate=2018-01-01")
  )
  expect_equal(
    modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", startDate = "2018-01-01", pageSize = 100),
    paste0(baseurl, "api/events.json?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=100&startDate=2018-01-01")
  )
})
