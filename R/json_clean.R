resp_json_clean <- function(resp) {
  l_init <-
    resp %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON(flatten = TRUE)

  d_init <-
    l_init %>%
    purrr::pluck("data") %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(mal_id = mal_id) %>%
    dplyr::relocate(mal_id)

  d_dims_init <-
    d_init %>%
    dplyr::select(mal_id, where(is.list))

  v_original_col_names <-
    d_dims_init %>%
    colnames() %>%
    stringr::str_subset(
      pattern = "^mal_id$",
      negate = TRUE
    )

  d_dims_init <-
    d_dims_init %>%
    dplyr::rename_with(~ stringr::str_remove(.x, "s$"))

  if (nrow(d_dims_init) > 1) {
    max_col <- ncol(d_dims_init) - 1
    i <- 1

    l_dim_tables <- list()

    while (i <= max_col) {
      object <- names(d_dims_init)[i + 1]

      d_step_1 <-
        d_dims_init %>%
        dplyr::select(1, i + 1) %>%
        tidyr::unnest(
          2,
          # keep_empty = TRUE,
          names_sep = "_"
        )

      l_dim_tables[[i]] <- d_step_1

      i <- i + 1
    }

    l_dim_tables <- purrr::set_names(l_dim_tables, names(d_dims_init)[-1])

    d_final <-
      d_init %>%
      dplyr::select(-dplyr::all_of(v_original_col_names)) %>%
      janitor::clean_names()

  } else {
    d_final <- d_init
    dim_tables <- NULL
  }

  l_final <- list(
    d = d_final,
    dim_tables = l_dim_tables
  )

  return(l_final)
}
