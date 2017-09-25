testthat::context("biosys_datasets")

testthat::test_that("biosys_datasets returns data filtered by project_id", {
    # data("projects_api")
    p <- biosys_datasets(project_id = 6)
    testthat::expect_true(tibble::is.tibble(p))
})

testthat::test_that("biosys_datasets returns all datasets", {
    # data("projects_api")
    p <- biosys_datasets()
    testthat::expect_true(tibble::is.tibble(p))
})

testthat::test_that("biosys_datasets takes un and pw", {
    # data("projects_api")
    p <- biosys_datasets(project_id = 6,
                        un = Sys.getenv("BIOSYS_UN"),
                        pw = Sys.getenv("BIOSYS_PW"))
    testthat::expect_true(tibble::is.tibble(p))
})
