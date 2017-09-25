#' Parse BioSys records to tibble
#'
#' @param un (character) The BioSys username, default \code{Sys.getenv("BIOSYS_UN")}
#' @param pw (character) The BioSys password, default \code{Sys.getenv("BIOSYS_PW")}
#' @param project_id (int) An optional project ID to restrict the result to.
#' @return A tibble of BioSys records with some common metadata fields plus the
#'   respective data.
#' @export
biosys_records <- function(
    un=Sys.getenv("BIOSYS_UN"),
    pw=Sys.getenv("BIOSYS_PW"),
    project_id = NULL){
    . <- ""
    data <- "silence R CMD check warning"
    if (!is.null(project_id)) {
        query = list("dataset__project__id" = project_id)
    } else {
        query = list()
    }

    biosys_records <- biosys_get(
        "records",
        un = un,
        pw = pw,
        query = query)$content %>% {
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
        dplyr::select(-data)  # R CMD check can't handle unquoted variable names

    datacols <- biosys_records$data %>%
        kimisc::list_to_df(.) %>%
        magrittr::extract2("value") %>%
        purrr::transpose(.) %>%
        tibble::as_tibble(.)

    cbind(metadatacols, datacols) %>% tibble::as_tibble(.)
}