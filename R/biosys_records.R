#' Parse BioSys records to tibble
#' @export
biosys_records <- function(
    un=Sys.getenv("BIOSYS_UN"),
    pw=Sys.getenv("BIOSYS_PW"),
    project_id = NULL){

    if (!is.null(project_id)) {
        query = list("dataset__project__id" = project_id)
    } else {
        query = list()
    }
    biosys_json <- biosys_get(
        "records", un = un, pw = pw, query = query)$content

    biosys_records <- biosys_json %>% {
        tibble::tibble(
            id = purrr::map_chr(., "id"),
            datetime = map_chr_hack(., "datetime"),
            species_name = map_chr_hack(., "species_name"),
            name_id = map_chr_hack(., "name_id"),
            file_name = purrr::map(., "source_info") %>%
                map_chr_hack(magrittr::extract2, 1),
            file_row = purrr::map(., "source_info") %>%
                map_chr_hack(magrittr::extract2, 2),
            last_modified = purrr::map_chr(., "last_modified"),
            dataset = purrr::map_chr(., "dataset"),
            site = map_chr_hack(., "site"),
            data = purrr::map(., "data")
        )}

    metadatacols <- biosys_records %>%
        dplyr::select(-data)

    datacols <- biosys_records$data %>%
        kimisc::list_to_df(.) %>%
        magrittr::extract2("value") %>%
        purrr::transpose(.) %>%
        tibble::as_tibble(.)

    cbind(metadatacols, datacols)
}