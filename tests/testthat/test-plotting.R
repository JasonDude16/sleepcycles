library(testthat)

test_that("Plot functions with dude, single-subject return ggplot objects", {
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

test_that("Plot functions with dude, multi-subject return ggplot objects", {
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
  p <- plot_hypnogram(result, id = 1)
  expect_s3_class(p, "ggplot")

  p <- plot_densities(result, id = 1)
  expect_s3_class(p, "ggplot")

  p <- plot_cycles(result, id = 1)
  expect_s3_class(p, "ggplot")

  p <- plot_summary(result, id = 1)
  expect_s3_class(p, "grob")

  p <- plot_cycle_proportions(result)
  expect_s3_class(p, "ggplot")

  p <- plot_cycle_counts(result)
  expect_s3_class(p, "ggplot")

  p <- plot_ids(result)
  expect_s3_class(p, "ggplot")
})

test_that("Plot functions with feinberg, single-subject return ggplot objects", {
  data("hypnogram_single", package = "sleepcycles")
  result <- suppressWarnings(
    sleepcycles_from_hypnogram(
      hypnogram_single,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "feinberg"
    )
  )
  p <- plot_hypnogram(result)
  expect_s3_class(p, "ggplot")

  p <- plot_cycles(result)
  expect_s3_class(p, "ggplot")

  p <- plot_summary(result)
  expect_s3_class(p, "grob")
})

test_that("Plot functions with feinberg, multi-subject return ggplot objects", {
  data("hypnogram_grouped", package = "sleepcycles")
  result <- suppressWarnings(
    sleepcycles_from_hypnogram(
      hypnogram_grouped,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "feinberg",
      id_col = "id",
      verbose = FALSE
    )
  )
  p <- plot_hypnogram(result, id = 1)
  expect_s3_class(p, "ggplot")

  p <- plot_cycles(result, id = 1)
  expect_s3_class(p, "ggplot")

  p <- plot_summary(result, id = 1)
  expect_s3_class(p, "grob")

  p <- plot_cycle_proportions(result)
  expect_s3_class(p, "ggplot")

  p <- plot_cycle_counts(result)
  expect_s3_class(p, "ggplot")

  p <- plot_ids(result)
  expect_s3_class(p, "ggplot")
})
