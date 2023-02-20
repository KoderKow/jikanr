get_anime_by <- function(mal_id, by) {
  checkmate::assert_numeric(
    x = mal_id,
    any.missing = FALSE,
    len = 1
  )

  rlang::arg_match(
    arg = by,
    values = c("full", "characters", "staff", "episodes", "news", "forum", "videos", "videos/episodes", "pictures", "statistics", "moreinfo", "recommendations", "userupdates", "reviews", "relations", "themes", "external", "streaming")
  )

  req <-
    get_anime(clean_json = FALSE) %>%
    httr2::req_url_path_append(mal_id) %>%
    httr2::req_url_path_append(by)

  resp <-
    req %>%
    httr2::req_perform()

  d <-
    resp %>%
    resp_json_clean()

  return(d)
}
