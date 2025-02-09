library(testthat)

# Dude ----------------------------------------------------------------------------------------

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

test_that("sleepcycles_from_hypnogram-multiple-dude works correctly", {
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
  expect_s3_class(result, "SleepCycle")
  expect_true("epoch" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("info" %in% names(result))
  expect_true(!is.null(result$epoch))
  expect_true(!is.null(result$summary))
  expect_true(result$info$is_grouped)
})

test_that("sleepcycles_from_hypnogram-dude handles REM only", {
  df <- data.frame(epoch = 1:50, stage = rep("R", 50))
  warnings <- suppressWarnings({
    captured_warnings <- character()
    result <- withCallingHandlers(
      sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage", method = "dude"),
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    captured_warnings
  })
  expected_warnings <- c(
    "One of the combo levels (N2) is not present in hypnogram",
    "One of the combo levels (N3) is not present in hypnogram",
    "No NREMP periods were found"
  )
  expect_setequal(warnings, expected_warnings)
  expect_s3_class(result, "SleepCycle")
  expect_true("epoch" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("info" %in% names(result))
  expect_true(!is.null(result$epoch))
  expect_true(!is.null(result$summary))
})

test_that("sleepcycles_from_hypnogram-dude handles NREM only", {
  df <- data.frame(epoch = 1:50, stage = sample(c("N2", "N3"), size = 50, replace = TRUE))
  expect_warning(
    sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage", method = "dude"),
    "No REMP periods were found"
  )
})

test_that("sleepcycles_from_hypnogram-dude handles no detected events gracefully", {
  df <- data.frame(epoch = 1:50, stage = rep("N1", 50))
  warnings <- suppressWarnings({
    captured_warnings <- character()
    result <- withCallingHandlers(
      sleepcycles_from_hypnogram(df, epoch_col = "epoch", stage_col = "stage", method = "dude"),
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    captured_warnings
  })
  expected_warnings <- c(
    "One of the combo levels (N2) is not present in hypnogram",
    "One of the combo levels (N3) is not present in hypnogram",
    "No NREMP or REMP periods were found, skipping..."
  )
  expect_setequal(warnings, expected_warnings)
  expect_null(result)
})

test_that("sleepcycles_from_hypnogram-dude validates `options` list", {
  df <- data.frame(epoch = 1:50, stage = sample(c("N2", "N3"), size = 50, replace = TRUE))
  expect_error(
    sleepcycles_from_hypnogram(
      df,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude",
      options = list("bad_item" = 1)
    ), "all names in `options` list must be the following: sleep_levels, combos, NREMP, REMP, kernel"
  )
  expect_error(
    sleepcycles_from_hypnogram(
      df,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude",
      options = list("sleep_levels" = "N0")
    ), "`sleep_levels` must only contain levels that are in `stage_col`"
  )
  expect_error(
    sleepcycles_from_hypnogram(
      df,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude",
      options = list("combos" = list("N0" = "N0"))
    ), "`combos` must be a named list, and all list items must contain levels that are in `stage_col`"
  )
  expect_error(
    sleepcycles_from_hypnogram(
      df,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude",
      options = list("combos" = list("N0"))
    ), "`combos` must be a named list"
  )
  expect_error(
    sleepcycles_from_hypnogram(
      df,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude",
      options = list("NREMP" = list("Wrong" = 1))
    ), "Valid option names for `NREMP` are: density_col, threshold, min_gap, min_size"
  )
  expect_error(
    sleepcycles_from_hypnogram(
      df,
      epoch_col = "epoch",
      stage_col = "stage",
      method = "dude",
      options = list("REMP" = list("Wrong" = 1))
    ), "Valid option names for `REMP` are: density_col, threshold, min_gap, min_size"
  )
})

# Feinberg ------------------------------------------------------------------------------------

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

test_that("sleepcycles_from_hypnogram-multiple-feinberg works correctly", {
  data("hypnogram_grouped", package = "sleepcycles")
  result <- sleepcycles_from_hypnogram(
    hypnogram_grouped,
    epoch_col = "epoch",
    stage_col = "stage",
    method = "feinberg",
    id_col = "id",
    verbose = FALSE
  )
  expect_s3_class(result, "SleepCycle")
  expect_true("epoch" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("info" %in% names(result))
  expect_true(!is.null(result$epoch))
  expect_true(!is.null(result$summary))
  expect_true(result$info$is_grouped)
})

# ---------------------------------------------------------------------------------------------

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
