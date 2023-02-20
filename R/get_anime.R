get_anime <- function(..., clean_json = TRUE) {
  params <- list(...)

  req <-
    base_url %>%
    httr2::request() %>%
    httr2::req_url_path_append("anime") %>%
    httr2::req_url_query(!!!params) %>%
    httr2::req_throttle(1) %>%
    httr2::req_error(body = error_body)

  if (clean_json) {
    ret <-
      resp %>%
      resp_json_clean()
  } else {
    ret <- req
  }

  return(ret)
}
