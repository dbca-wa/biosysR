#' Parse BioSys projects to tibble
#' @export
biosys_projects <- function(
    un=Sys.getenv("BIOSYS_UN"),
    pw=Sys.getenv("BIOSYS_PW")){

    biosys_get(
        "projects",
        un = un,
        pw = pw)$content %>% {
            tibble::tibble(
                id = purrr::map_chr(., "id"),
                name = purrr::map_chr(., "name"),
                code = purrr::map_chr(., "code"),
                description = map_chr_hack(., "description"),
                site_count = purrr::map_int(., "site_count"),
                dataset_count = purrr::map_int(., "dataset_count"),
                record_count = purrr::map_int(., "record_count"),
                longitude = purrr::map(., c("centroid", "coordinates")) %>%
                    map_dbl_hack(magrittr::extract2, 1),
                latitude = purrr::map(., c("centroid", "coordinates")) %>%
                    map_dbl_hack(magrittr::extract2, 2),
                datum = purrr::map_chr(., "datum"),
                timezone = purrr::map_chr(., "timezone")
                # site_data_package
                # custodians
            )
        }
}