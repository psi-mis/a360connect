test_that("Events endpoint correctly generates the url", {
  expect_error(events_endpoint())
  expect_error(events_endpoint(startDate = "2018-01-01",))
  expect_error(events_endpoint(baseurl = "2018-01-01"))
  expect_equal(events_endpoint(baseurl = "https://clone.psi-mis.org/", program_id = "Ymn7RO0lkf7"),
               "https://clone.psi-mis.org/api/events?paging=false&program=Ymn7RO0lkf7")
})
