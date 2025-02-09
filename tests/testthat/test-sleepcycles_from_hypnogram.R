library(testthat)

test_that("sleepcycles_from_hypnogram-single-dude works correctly", {
  data("hypnogram_single", package = "sleepcycles")
  result <- suppressWarnings(
    sleepcycles_from_hypnogram(
      hypnogram_single,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude"
    )
  )
  expect_s3_class(result, "SleepCycle")
  expect_true("epoch" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("info" %in% names(result))
  expect_true(!is.null(result$epoch))
  expect_true(!is.null(result$summary))
  expect_true(!result$info$is_grouped)
})

test_that("sleepcycles_from_hypnogram-single-feinberg works correctly", {
  data("hypnogram_single", package = "sleepcycles")
  result <- sleepcycles_from_hypnogram(
    hypnogram_single,
    epoch_col = "epoch",
    stage_col = "stage",
    method = "feinberg"
  )
  expect_s3_class(result, "SleepCycle")
  expect_true("epoch" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("info" %in% names(result))
  expect_true(!is.null(result$epoch))
  expect_true(!is.null(result$summary))
  expect_true(!result$info$is_grouped)
})

test_that("sleepcycles_from_hypnogram-multiple-dude works correctly", {
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
  expect_s3_class(result, "SleepCycle")
  expect_true("epoch" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("info" %in% names(result))
  expect_true(!is.null(result$epoch))
  expect_true(!is.null(result$summary))
  expect_true(result$info$is_grouped)
})

test_that("sleepcycles_from_hypnogram-multiple-feinberg works correctly", {
  data("hypnogram_grouped", package = "sleepcycles")
  result <- sleepcycles_from_hypnogram(
    hypnogram_grouped,
    epoch_col = "epoch",
    stage_col = "stage",
    method = "feinberg",
    id_col = "id"
  )
  expect_s3_class(result, "SleepCycle")
  expect_true("epoch" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("info" %in% names(result))
  expect_true(!is.null(result$epoch))
  expect_true(!is.null(result$summary))
  expect_true(result$info$is_grouped)
})

test_that("sleepcycles_from_hypnogram does not allow missing values in stage_col", {
  df <- data.frame(epoch = c(1, 2, 3), stage = c("W", NA, "W"))
  expect_error(
    sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage"),
    "stage contains missing values, please fix."
  )
})

test_that("sleepcycles_from_hypnogram must have valid levels in stage_col", {
  df <- data.frame(epoch = c(1, 2, 3), stage = c("W", "B", "W"))
  expect_error(
    sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage"),
    "stage contains invalid levels: B"
  )
})

test_that("sleepcycles_from_hypnogram cannot have missing epochs", {
  df <- data.frame(epoch = c(1, NA, 3), stage = rep("W", 3))
  expect_error(
    sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage"),
    "epoch has missing values, please fix."
  )
})

test_that("sleepcycles_from_hypnogram does not have duplicated epochs", {
  df <- data.frame(epoch = c(1, 1, 2), stage = rep("W", 3))
  expect_error(
    sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage"),
    "epoch contains duplicated epochs, please fix."
  )
})

test_that("sleepcycles_from_hypnogram must have ordered epochs", {
  df <- data.frame(epoch = c(3, 2, 1), stage = rep("W", 3))
  expect_error(
    sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage"),
    "Some epochs are not ordered, please fix."
  )
})

test_that("sleepcycles_from_hypnogram must have contiguous epochs", {
  df <- data.frame(epoch = c(1, 3, 4), stage = rep("W", 3))
  expect_error(
    sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage"),
    "Not all epochs are contiguous, please fix."
  )
})
