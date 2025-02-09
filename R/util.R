#' Extract a Single Subject's Data from a Grouped SleepCycle Object
#'
#' This function extracts the **sleep cycle data** for a specific subject from a grouped `SleepCycle` object.
#'
#' @param sleepcycle_obj An object of class `"SleepCycle"` that contains **multiple IDs**.
#' @param id The subject ID to extract from `sleepcycle_obj`. Must be present in the grouped dataset.
#'
#' @return A new `"SleepCycle"` object containing data **only for the selected subject**, including:
#'   - `epoch`: Per-epoch sleep stage and cycle classifications.
#'   - `summary`: Sleep cycle summaries for the extracted subject.
#'   - `info`: Metadata about the `SleepCycle` object.
#'
#' @details
#' This function is used to **extract a single subject's sleep cycle data** from a **multi-subject dataset**.
#' If the provided `sleepcycle_obj` is **not grouped**, the function stops execution to prevent unnecessary extraction.
#'
#' @export
get_id <- function(sleepcycle_obj, id) {

  if (!inherits(sleepcycle_obj, "SleepCycle")) {
    stop("The first argument (sleepcycle_obj) must be of class `SleepCycle`.")
  }
  x <- sleepcycle_obj

  if (!x$info$is_grouped) {
    stop("SleepCycle object is not grouped, no need to extract an id!")
  }

  x_epoch <- dplyr::filter(x$epoch, .data[[x$info$id_col]] == id)
  x_epoch[[x$info$stage_col]] <- droplevels(x_epoch[[x$info$stage_col]])

  x <- list(
    "epoch" = x_epoch,
    "summary" = dplyr::filter(x$summary, .data[[x$info$id_col]] == id),
    "info" = c("is_grouped" = FALSE, x$info[-1])
  )

  class(x) <- "SleepCycle"
  return(x)
}


#' Handle Grouped SleepCycle Objects
#'
#' This internal function extracts a specific subject's sleep data from a grouped `SleepCycle` object.
#' If the object is **not grouped**, it returns the full dataset unchanged.
#'
#' - If `x` is **grouped**, it extracts the sleep data for the specified `id`.
#' - If `id` is `NULL`, it defaults to the **first available subject** and issues a warning.
#' - If `id` is not found, it throws an error.
#'
#' @param x A `SleepCycle` object.
#' @param id (Optional) The subject ID to extract. If `NULL`, the first subject is used.
#'
#' @return A **list** containing:
#'   - `"epoch"`: Per-epoch sleep staging data.
#'   - `"summary"`: Summary statistics for the subject (or full dataset if not grouped).
#'   - `"info"`: Metadata about the `SleepCycle` object.
#'
#' @details
#' This function is used internally by plotting and processing functions that need to extract
#' data for a single subject when working with grouped `SleepCycle` objects.
#'
#' @keywords internal
#' @rdname handle_grouped_obj
.handle_grouped_obj <- function(x, id) {

  if (x$info$is_grouped) {
    ids <- unique(x$epoch[[x$info$id_col]])
    if (is.null(id)) {
      id <- ids[1]
      warning("No id was given, defaulting to first (", id, ")")
    } else {
      if (id %in% ids == FALSE) {
        stop(id, " not found in dataset!")
      }
    }
    return(get_id(x, id))

  } else {
    x_epoch <- x$epoch
    x_summary <- x$summary
    return(list("epoch" = x_epoch, "summary" = x_summary, "info" = x$info))

  }
}
