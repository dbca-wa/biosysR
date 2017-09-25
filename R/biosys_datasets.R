#' Parse BioSys datasets to tibble
#'
#' @param un (character) The BioSys username, default \code{Sys.getenv("BIOSYS_UN")}
#' @param pw (character) The BioSys password, default \code{Sys.getenv("BIOSYS_PW")}
#' @param project_id (int) An optional project ID to restrict the result to.
#' @return A tibble of dataset metadata. The field \code{data_package} is a list
#'   column containing the \code{tabular_data_package} definition.
#' @export
biosys_datasets <- function(
    un=Sys.getenv("BIOSYS_UN"),
    pw=Sys.getenv("BIOSYS_PW"),
    project_id = NULL){
    . <- ""
    if (!is.null(project_id)) {
        query = list("project__id" = project_id)
    } else {
        query = list()
    }

    biosys_get(
        "datasets",
        un = un,
        pw = pw,
        query = query)$content %>% {
            tibble::tibble(
                id = purrr::map_chr(., "id"),
                record_count = purrr::map_int(., "record_count"),
                data_package = purrr::map(., "data_package"),
                name = purrr::map_chr(., "name"),
                type = purrr::map_chr(., "type"),
                description = map_chr_hack(., "description"),
                project_id = purrr::map_int(., "project")
            )
        }
}