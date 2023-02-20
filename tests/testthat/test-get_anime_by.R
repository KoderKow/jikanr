test_that("get anime by", {
  d <- get_anime_by(666, "characters")

  checkmate::expect_tibble(
    x = d,
    any.missing = FALSE,
    nrows = 13,
    ncols = 8,
    null.ok = FALSE
  )
})
