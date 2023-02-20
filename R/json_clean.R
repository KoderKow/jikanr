resp_json_clean <- function(resp) {
  d <- tryCatch(
    expr = {
      resp %>%
        httr2::resp_body_string() %>%
        tidyjson::enter_object(data) %>%
        tidyjson::gather_array() %>%
        tidyjson::spread_all()
    },
    error = function(e) {
      resp %>%
        httr2::resp_body_string() %>%
        tidyjson::enter_object(data) %>%
        tidyjson::spread_all()
    }
  ) %>%
    # Remove ..JSON column
    dplyr::as_tibble() %>%
    ## Remove other columns created by tidyjson
    dplyr::select(
      -dplyr::contains("document.id"),
      -dplyr::contains("array.index")
    ) %>%
    janitor::clean_names()

  return(d)
}
