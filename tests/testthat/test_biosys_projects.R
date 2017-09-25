testthat::context("biosys_projects")

testthat::test_that("biosys_projects returns data", {
    # data("projects_api")
    p <- biosys_projects()
    testthat::expect_true(tibble::is.tibble(p))
})

testthat::test_that("biosys_projects takes un and pw", {
    # data("projects_api")
    p <- biosys_projects(un = Sys.getenv("BIOSYS_UN"),
                         pw = Sys.getenv("BIOSYS_PW"))
    testthat::expect_true(tibble::is.tibble(p))
})
