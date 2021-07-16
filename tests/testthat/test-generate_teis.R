test_that("Events endpoint correctly generates the url", {
  expect_error(modify_events_endpoint())
  expect_error(modify_events_endpoint(startDate = "2018-01-01",))
  expect_error(modify_events_endpoint(baseurl = baseurl))
  expect_equal(modify_events_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7"),
               "https://clone.psi-mis.org/api/events?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50")
})


