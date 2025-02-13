library(testthat)

test_that("sleepcycles_from_hypnogram-multiple-shiny app works", {
  data("hypnogram_grouped", package = "sleepcycles")
  result <- suppressWarnings(
    sleepcycles_from_hypnogram(
      hypnogram_grouped,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude",
      id_col = "id",
      verbose = FALSE
    )
  )
  result <- run_sleepcycles_app(result)
  expect_s3_class(result, "shiny.appobj")
})
