testthat::context("map_chr_hack")

testthat::test_that("map_chr_hack returns text when text found", {
    data("projects_api")
    testthat::expect_equal(
        map_chr_hack(projects_api, "description"),
        paste0("Benthic community composition determined by point count of drop",
               " camera images using the software EcoPAAS.\n\nArea: Shark Bay\n",
               "Main habitat: Seagrass\nDate surveyed: March 2016"))

    testthat::expect_true(is.na(map_chr_hack(projects_api, "site_data_package")))
})

testthat::test_that("map_chr_hack returns NA when NA found", {
    data("projects_api")
    testthat::expect_equal(
        map_chr_hack(projects_api, "description"),
        paste0("Benthic community composition determined by point count of drop",
               " camera images using the software EcoPAAS.\n\nArea: Shark Bay\n",
               "Main habitat: Seagrass\nDate surveyed: March 2016"))

    testthat::expect_true(is.na(map_chr_hack(projects_api, "site_data_package")))
})
