#' Extract Sleep Cycles from a Hypnogram
#'
#' This function detects sleep cycles from a **hypnogram** using one of two algorithms:
#' - **DUDE** (Density-Based Underlying Dynamics of Epochs): Uses sleep stage densities to define sleep cycles.
#' - **Feinberg** (Classic Feinberg & Floyd method): Uses rule-based classification for defining cycles.
#'
#' It processes **both single-subject and multi-subject data**. For grouped data (multiple IDs), it applies the selected method separately to each subject.
#'
#' @param df A data frame containing sleep staging data.
#' @param epoch_col A string specifying the column name for sleep epochs (must be numeric and sequential).
#' @param stage_col A string specifying the column name representing sleep stages.
#' @param method The algorithm used for detecting sleep cycles. Options:
#'   - `"dude"` (default) - Uses density-based clustering for defining cycles.
#'   - `"feinberg"` - Uses traditional rule-based heuristics for cycle segmentation.
#' @param options A named list containing algorithm-specific parameters. If left empty, default parameters are used.
#'   - **DUDE Algorithm Options**:
#'     - `"sleep_levels"`: Vector of valid sleep stages (default: `c("N1", "N2", "N3", "R")`).
#'     - `"combos"`: Defines merged stages (e.g., `list("N2_N3" = c("N2", "N3"))`).
#'     - `"NREMP"`: Named list defining **NREM Period (NREMP) detection** parameters:
#'         - `"density_col"`: Column used for NREMP detection (default: `"N2_N3"`).
#'         - `"threshold"`: Threshold for cycle classification (default: `0.5`).
#'         - `"min_gap"`: Minimum gap between cycles (default: `20` epochs).
#'         - `"min_size"`: Minimum cycle duration (default: `20` epochs).
#'     - `"REMP"`: Named list defining **REM Period (REMP) detection** parameters:
#'         - `"density_col"`: Column used for REMP detection (default: `"R"`).
#'         - `"threshold"`: Threshold for cycle classification (default: `0.5`).
#'         - `"min_gap"`: Minimum gap between cycles (default: `15` epochs).
#'         - `"min_size"`: Minimum cycle duration (default: `10` epochs).
#'     - `"kernel"`: A Gaussian kernel used for smoothing sleep stage transitions.
#'
#'   - **Feinberg Algorithm Options**:
#'     - `"sleepstart"`: Defines when sleep begins (`"N1"` or `"N2"`, default: `"N1"`).
#'     - `"treat_as_W"`: Defines which stage should be treated as wakefulness (default: `"A"`).
#'     - `"rm_incomplete_period"`: Whether to remove incomplete cycles (default: `FALSE`).
#'     - `"REMP_length"`: Minimum REM duration for it to be considered a valid cycle (default: `10` epochs).
#'     - `"sleep_levels"`: Vector of sleep stages included in cycles (default: `c("N1", "N2", "N3", "R")`).
#'
#' @param id_col (Optional) A string specifying the column name for subject IDs. If provided, the function processes each subject separately.
#' @param verbose Logical. If `TRUE` (default), the function prints warnings and messages when issues occur in grouped data.
#'
#' @return An object of class `"SleepCycle"`, which is a list containing:
#'   - `epoch`: A data frame with per-epoch cycle classifications.
#'   - `summary`: A summary of detected cycles with their start and end epochs.
#'   - `info`: A list of metadata including the method used, selected options, and sleep stage levels.
#'
#' @details
#' The function first validates the input hypnogram using `check_hypnogram()`, ensuring correct format and no missing values.
#' Then, based on the chosen **method**, it calls either `.sleepcycles_from_hypnogram_dude()` or `.sleepcycles_from_hypnogram_feinberg()` to process the data.
#'
#' If `id_col` is provided, the function runs the cycle detection **independently for each subject** and combines the results.
#'
#' @references
#' Feinberg, I., March, J. D., Floyd, T. C., Jimison, R., Bossom-Demitrack, L., & Katz, P. H. (1978).
#' Sleep cycle dynamics and REM sleep regulation. *Science, 201*(4352), 625-627.
#' \doi{10.1126/science.663657}
#'
#' @importFrom rlang .data
#'
#' @export
sleepcycles_from_hypnogram <- function(df, epoch_col, stage_col, method = "dude", options = list(), id_col = NULL, verbose = TRUE) {

  stopifnot(method %in% c("dude", "feinberg"))

  if (!is.null(id_col)) {
    if (!any(is.character(df[[id_col]]), !is.factor(df[[id_col]]))) {
      stop("The ID column must be a character or factor")
    }
  }

  check_hypnogram(df = df, epoch_col = epoch_col, stage_col = stage_col, id_col = id_col, on_error = "stop")

  valid_levels = c("N3", "N2", "N1", "R", "W", "A")
  if (!is.factor(df[[stage_col]])) {
    lvls <- intersect(valid_levels, as.factor(df[[stage_col]]))
    df[[stage_col]] <- forcats::fct_relevel(as.factor(df[[stage_col]]), lvls)
  }

  if (method == "dude") {
    FUN <- .sleepcycles_from_hypnogram_dude

  } else if (method == "feinberg") {
    FUN <- .sleepcycles_from_hypnogram_feinberg

  }

  if (is.null(id_col)) {
    res <- FUN(
      df = df,
      epoch_col = epoch_col,
      stage_col = stage_col,
      options = options
    )
    if (is.null(res)) {
      return(NULL)
    }
  } else {
    ids <- unique(df[[id_col]])
    df_list <- vector(mode = "list", length = length(ids))
    names(df_list) <- ids
    for (.id in ids) {
      df_subset <- dplyr::filter(df, .data[[id_col]] == .id)
      df_list[[.id]] <- withCallingHandlers(
        FUN(
          df = df_subset,
          epoch_col = epoch_col,
          stage_col = stage_col,
          options = options
        ),
        warning = function(w) {
          if (verbose) {
            message("[ID: ", .id, "] ", conditionMessage(w))
          }
        }
      )
    }

    if (length(df_list) == 0) {
      return(NULL)
    }

    res <- list(
      "epoch" = purrr::list_rbind(purrr::map(df_list, ~.x$epoch), names_to = id_col),
      "summary" = purrr::list_rbind(purrr::map(df_list, ~.x$summary), names_to = id_col),
      "info" = purrr::compact(df_list)[[1]]$info
    )
  }

  obj <- list(
    "epoch" = res$epoch,
    "summary" = res$summary,
    "info" = list(
      "is_grouped" = if (is.null(id_col)) FALSE else TRUE,
      "id_col" = id_col,
      "epoch_col" = epoch_col,
      "stage_col" = stage_col,
      "stage_levels" = levels(df[[stage_col]]),
      "method" = method,
      "method_opts" = res$info
    )
  )
  class(obj) <- "SleepCycle"
  return(obj)

}

.check_hypnogram_base <- function(df, epoch_col, stage_col, valid_levels = c("N3", "N2", "N1", "R", "W", "A")) {

  stopifnot(
    is.data.frame(df),
    nrow(df) > 0,
    is.character(epoch_col),
    length(epoch_col) == 1,
    is.character(stage_col),
    length(stage_col) == 1
  )

  if (stage_col %in% colnames(df) == FALSE) {
    stop(stage_col, " is not a column in the data frame.")
  }

  if (any(is.na(df[[stage_col]]))) {
    stop(stage_col, " contains missing values, please fix.")
  }

  if (!all(unique(df[[stage_col]]) %in% valid_levels)) {
    invalid_levels <- setdiff(unique(df[[stage_col]]), valid_levels)
    stop(stage_col, " contains invalid levels: ", paste0(invalid_levels, collapse = ", "))
  }

  if (epoch_col %in% colnames(df) == FALSE) {
    stop(epoch_col, " is not a column in the data frame.")
  }

  if (!is.numeric(df[[epoch_col]])) {
    stop(epoch_col, " must be numeric.")
  }

  if (any(is.na(df[[epoch_col]]))) {
    stop(epoch_col, " has missing values, please fix.")
  }

  if (any(duplicated(df[[epoch_col]]))) {
    stop(epoch_col, " contains duplicated epochs, please fix.")
  }

  if (!all(diff(df[[epoch_col]]) > 0)) {
    stop("Some epochs are not ordered, please fix.")
  }

  if (!all(diff(df[[epoch_col]]) == 1)) {
    stop("Not all epochs are contiguous, please fix.")
  }

}

#' Validate a Hypnogram Data Frame
#'
#' This function checks the structure and validity of a hypnogram dataset. It ensures:
#' - The presence of required columns (`epoch_col`, `stage_col`).
#' - The correct format of `epoch_col` (numeric, ordered, contiguous).
#' - The correct format of `stage_col` (valid sleep stages, no missing values).
#' - If `id_col` is provided, validation is performed separately for each subject.
#'
#' @param df A data frame containing the hypnogram.
#' @param epoch_col The column name that represents sleep epochs (must be numeric and sequential).
#' @param stage_col The column name representing sleep stages.
#' @param id_col (Optional) A column representing subject IDs for grouped data. If provided, validation runs separately for each subject.
#' @param on_error Defines error handling behavior:
#'   - `"stop"` (default) will halt execution on errors.
#'   - `"message"` will print warnings instead of stopping execution. This behavior allows the user to see errors for all ids when working with a grouped hypnogram.
#'
#' @return Returns `NULL` if validation is successful. Otherwise, it stops or prints an error message.
#' @export
check_hypnogram <- function(df, epoch_col, stage_col, id_col = NULL, on_error = "stop") {

  stopifnot(on_error %in% c("stop", "message"))

  if (is.null(id_col)) {
    if (on_error == "stop") {
      .check_hypnogram_base(df = df, epoch_col = epoch_col, stage_col = stage_col)
    } else if (on_error == "message") {
      tryCatch(
        .check_hypnogram_base(df = df, epoch_col = epoch_col, stage_col = stage_col),
        error = function(e) message(e)
      )
    }
  } else {
    if (id_col %in% colnames(df) == FALSE) {
      stop(id_col, " not found in data frame.")
    }
    ids <- unique(df[[id_col]])
    for (.id in ids) {
      df_subset <- dplyr::filter(df, .data[[id_col]] == .id)

      if (on_error == "stop") {
        tryCatch(
          .check_hypnogram_base(
            df = df_subset,
            epoch_col = epoch_col,
            stage_col = stage_col
          ),
          error = function(e) stop("[ID: ", .id, "] ", conditionMessage(e), call. = FALSE)
        )
      } else if (on_error == "message") {
        tryCatch(
          .check_hypnogram_base(
            df = df_subset,
            epoch_col = epoch_col,
            stage_col = stage_col
          ),
          error = function(e) message("[ID: ", .id, "] ", conditionMessage(e))
        )
      }
    }
  }
}


.cycle_summary <- function(x, df, epoch_col, stage_col, options, ...) {
  sleep_indices <- which(df[[stage_col]] %in% options$sleep_levels)

  first_sleep <- sleep_indices[1]
  last_sleep <- sleep_indices[length(sleep_indices)]

  first_sleep_epoch <- df[[epoch_col]][first_sleep]
  last_sleep_epoch <- df[[epoch_col]][last_sleep]

  res <- x |>
    dplyr::mutate(n_epochs = .data$end_epoch - .data$start_epoch + 1) |>
    dplyr::filter(...) |>
    dplyr::group_by(.data$cycle_type) |>
    dplyr::mutate(cycle = 1:dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      first_sleep_epoch = first_sleep_epoch,
      last_sleep_epoch = last_sleep_epoch,
      sleep_dur = (last_sleep - first_sleep) + 1,
      start_prop = (.data$start_epoch - first_sleep_epoch + 1) / .data$sleep_dur,
      end_prop = (.data$end_epoch - first_sleep_epoch + 1) / .data$sleep_dur
    ) |>
    dplyr::select(dplyr::all_of("cycle"), dplyr::everything())

  return(res)
}

#' Custom Print Method for SleepCycle Objects
#'
#' @param x A SleepCycle object.
#' @param ... not currently implemented.
#' @export
print.SleepCycle <- function(x, ...) {
  gray_text <- crayon::make_style("gray60")
  print(x[c("epoch", "summary")])
  cat("$info")
  cat(
    gray_text(
      "\n<info list> \n",
      paste0("is_grouped: ", x$info$is_grouped, "\n"),
      paste0("id_col: ", if(is.null(x$info$id_col)) "NULL" else x$info$id_col, "\n"),
      paste0("epoch_col: ", x$info$epoch_col, "\n"),
      paste0("stage_col: ", x$info$stage_col, "\n"),
      paste0("stage_levels: ", paste(x$info$stage_levels, collapse = " "), "\n"),
      paste0("method: ", x$info$method, "\n"),
      "method_opts <...> \n<info list>\n"
    )
  )
}
