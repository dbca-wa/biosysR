testthat::context("biosys_records")

testthat::test_that("biosys_records returns data filtered by project_id", {
    # data("projects_api")
    p <- biosys_records(project_id = 6)
    testthat::expect_true(tibble::is.tibble(p))
})

testthat::test_that("biosys_records takes un and pw", {
    # data("projects_api")
    p <- biosys_records(project_id = 6,
                        un = Sys.getenv("BIOSYS_UN"),
                        pw = Sys.getenv("BIOSYS_PW"))
    testthat::expect_true(tibble::is.tibble(p))
})
