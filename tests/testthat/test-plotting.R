library(testthat)

test_that("Plot functions with single-subject return ggplot objects", {
  data("hypnogram_single", package = "sleepcycles")
  result <- suppressWarnings(
    sleepcycles_from_hypnogram(
      hypnogram_single,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude"
    )
  )
  p <- plot_hypnogram(result)
  expect_s3_class(p, "ggplot")

  p <- plot_densities(result)
  expect_s3_class(p, "ggplot")

  p <- plot_cycles(result)
  expect_s3_class(p, "ggplot")

  p <- plot_summary(result)
  expect_s3_class(p, "grob")
})

test_that("Plot functions with multi-subject return ggplot objects", {
  data("hypnogram_grouped", package = "sleepcycles")
  result <- suppressWarnings(
    sleepcycles_from_hypnogram(
      hypnogram_grouped,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude",
      id_col = "id"
    )
  )
  p <- plot_hypnogram(result, id = 1)
  expect_s3_class(p, "ggplot")

  p <- plot_densities(result, id = 1)
  expect_s3_class(p, "ggplot")

  p <- plot_cycles(result, id = 1)
  expect_s3_class(p, "ggplot")

  p <- plot_summary(result, id = 1)
  expect_s3_class(p, "grob")
})





