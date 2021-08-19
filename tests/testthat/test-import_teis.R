sample_teis <- googlesheets4::read_sheet(
  "1D2tl8uD27YiPwcmd0Yqrzz6X8oGEbJ6KBl82l-nXQWA",
  sheet = "test")

# get tei schema
# tei_schema <- jsonlite::read_json(
# "./clone.psi-mis.org/api/schemas/trackedEntityInstance.json")



test_that("Generate TEI paylads returns a correct tei payload", {
  payload <- generate_tei_payload(sample_teis)
  expect_equal(
    names(payload),
    c(
      "trackedEntityType",
      "orgUnit",
      "trackedEntityInstance",
      "attributes",
      "enrollments"
    )
  )
  expect_equal(class(payload$attributes), "list")
  expect_equal(class(payload$enrollments), "list")
  expect_equal(nrow(payload), 2)
})

test_that("TEI payload generation triggers the expected errors", {
  # when any other object other than a single data.frame is passed to it.
  expect_error(generate_tei_payload(list(sample_teis)), "evnt must be a data.frame object")

  test_d <- sample_teis[,1:5] # without KEY column
  expect_error(generate_tei_payload(test_d), "evnt must have a special column,
           `KEY`, that uniquely identifys the events to generate TEIs")

  sample_teis <- data.table::as.data.table(sample_teis)
  test_d <- sample_teis[, -"TEI"]

  expect_error(generate_tei_payload(test_d),
               "evnt must have a special column, `TEI`, with the TEI ids")
})

test_that("TEI payload can be generated from different tracked entity attributes & mapping file", {
  payload <- generate_tei_payload(sample_teis,
                                  tea = selected_tea[1:2],
                                  tea_mapping = tea_map[tea_map$name %in% selected_tea[1:2],])

  # we expect only teis with with two attributes
  expect_equal(length(payload$attributes[[1]]$attribute), 2)
})

# start_capturing()
#
# get_tei("p8rm6hqPR8q")
#
# stop_capturing()

with_mock_api(
  test_that("Get TEI returns a TEI object", {
    d <- get_tei("p8rm6hqPR8q")
    expect_equal(d$status, 200L)
  })
)

start_capturing()
create_teis(sample_teis, baseurl = "https://staging.psi-mis.org/")
stop_capturing()

# start_capturing()
# update_teis(sample_teis, baseurl = "https://staging.psi-mis.org/")
# stop_capturing()


# test_that("TEIs are updated correctly", {
#   tei <- generate_tei_payload(sample_teis)
#
#   # tei_ls <- list(
#   #   trackedEntityType = tei$trackedEntityType[1],
#   #   trackedEntityInstance = tei$trackedEntityInstance[1],
#   #   orgUnit = tei$orgUnit[1],
#   #   attributes = tei$attributes[[1]],
#   #   enrollments = tei$enrollments[[1]]
#   # )
#   baseurl <- "https://staging.psi-mis.org/"
#
#   res <- httr::PUT(paste0(baseurl,
#                           "api/trackedEntityInstances/",
#                           tei$trackedEntityInstance[1],
#                           "?importStrategy=UPDATE"),
#     body = toJSON(
#       # list(
#       #   trackedEntityInstances = tei[1,]
#       # )
#       tei_ls,
#       auto_unbox = T
#     ), content_type_json(), authenticate(user, pass)
#   )
#
#   expect_equal(res$status, 200L)
#   print(res)
# })

with_mock_api(
  test_that("TEIs are created correctly", {
    d <- create_teis(sample_teis, baseurl = "https://staging.psi-mis.org/")
    expect_equal(d$response$status, 200L)
  })
)

with_mock_api(
  test_that("TEIs are updated correctly ", {
    d <- update_teis(sample_teis, baseurl = "https://staging.psi-mis.org/")
    expect_equal(d[[1]]$response$status, 200L)
    expect_equal(d[[1]]$content$response$importOptions$importStrategy, "UPDATE")
    expect_equal(d[[1]]$content$response$importOptions$ignoreEmptyCollection, TRUE)
  })
)

# start_capturing()
# res <- create_and_update_teis(sample_teis, baseurl = "https://staging.psi-mis.org/")
# stop_capturing()

with_mock_api(
  test_that("Test that create and update teis works correctly", {
    # the sample teis above should all be updated
    d <- create_and_update_teis(sample_teis, baseurl = "https://staging.psi-mis.org/")

    expect_equal(d[[1]][[1]]$response$status, 200L)
    expect_equal(d[[1]][[1]]$content$response$importOptions$importStrategy, "UPDATE")
    expect_equal(d[[1]][[1]]$content$response$importOptions$ignoreEmptyCollection, TRUE)

    expect_equal(d[[2]][[1]]$response$status, 200L)
    expect_equal(d[[2]][[1]]$content$response$importOptions$importStrategy, "UPDATE")
    expect_equal(d[[2]][[1]]$content$response$importOptions$ignoreEmptyCollection, TRUE)
  })
)




