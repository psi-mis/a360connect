with_mock_api(
  test_that("get evnt works as expected", {
    expect_error(get_evnt(baseurl, program_id = "Ymn7R"))
    expect_error(get_evnt())
    d <- get_evnt(baseurl, program_id = "Ymn7RO0lkf7")
    expect_s3_class(d, class = "psi-mis_api")
    expect_identical(class(d$content$events), "data.frame")
    expect_type(d$endpoint, "character")
    expect_identical(class(d$response), "response")
  })
)

test_that("evnt endpoint correctly generates the url", {
  expect_error(modify_evnt_endpoint())
  expect_error(modify_evnt_endpoint(startDate = "2018-01-01", ))
  expect_error(modify_evnt_endpoint(baseurl = baseurl))
  expect_equal(
    modify_evnt_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7"),
    "https://clone.psi-mis.org/api/evnt?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50"
  )
  expect_equal(
    modify_evnt_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", paging = F),
    "https://clone.psi-mis.org/api/evnt?paging=false&program=Ymn7RO0lkf7"
  )
  expect_equal(
    modify_evnt_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", startDate = "2018-01-01"),
    paste0(baseurl, "api/evnt?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50&startDate=2018-01-01")
  )
  expect_equal(
    modify_evnt_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", endDate = "2018-01-01"),
    paste0(baseurl, "api/evnt?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=50&endDate=2018-01-01")
  )
  expect_equal(
    modify_evnt_endpoint(baseurl = baseurl, program_id = "Ymn7RO0lkf7", startDate = "2018-01-01", pageSize = 100),
    paste0(baseurl, "api/evnt?paging=true&totalPages=true&program=Ymn7RO0lkf7&pageSize=100&startDate=2018-01-01")
  )
})
