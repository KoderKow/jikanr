error_body <- function(resp) {
  x <- resp %>%
    httr2::resp_body_json()

  glue("Type: {x$type}
       - Message: {x$message}
       - Error: {x$error}")
}
