test_that("Events endpoint correctly generates the url", {
  expect_error(modify_events_endpoint())
  expect_error(modify_events_endpoint(startDate = "2018-01-01",))
  expect_error(modify_events_endpoint(baseurl = baseurl))
  expect_equal(modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7"),
               "https://clone.psi-mis.org/api/events?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50")
  expect_equal(modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", paging = F),
               "https://clone.psi-mis.org/api/events?paging=false&program=Ymn7RO0lkf7")
  expect_equal(modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", startDate = "2018-01-01"),
               paste0(baseurl,"api/events?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50&startDate=2018-01-01"))
  expect_equal(modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", endDate = "2018-01-01"),
               paste0(baseurl,"api/events?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50&endDate=2018-01-01"))
  expect_equal(modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", startDate = "2018-01-01", pageSize = 100),
               paste0(baseurl,"api/events?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=100&startDate=2018-01-01"))
})


