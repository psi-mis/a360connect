sample_teis <- googlesheets4::read_sheet("1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA", sheet = "test")

# get tei schema
#tei_schema <- jsonlite::read_json("./clone.psi-mis.org/api/schemas/trackedEntityInstance.json")



test_that("Generate TEI paylads returns a correct tei payload",{
  payload <- generate_tei_payload(sample_teis)
  expect_equal(names(payload),
               c("trackedEntityType",
                 "trackedEntity",
                 "orgUnit",
                 "attributes",
                 "enrollments"
                 ))
  expect_equal(class(payload$attributes), "list")
  expect_equal(class(payload$enrollments), "list")
  expect_warning(generate_tei_payload(sample_teis), "skipping teis missing enrollment")
  expect_equal(nrow(payload), 2)
})

# start_capturing()
#
# get_tei("p8rm6hqPR8q")
#
# stop_capturing()

with_mock_api(
  test_that("Get TEI returns a TEI object",{
    d <- get_tei("p8rm6hqPR8q")
    expect_equal(d$status, 200L)
  })
)

# start_capturing()
# create_teis(sample_teis, baseurl = "https://staging.psi-mis.org/")
# update_teis(sample_teis, baseurl = "https://staging.psi-mis.org/")
# stop_capturing()
#
# test_that("TEIs are updated correctly", {
#
# })

