#' Example Hypnogram Datasets
#'
#' These datasets contain example hypnograms used for testing sleep cycle detection functions.
#'
#' - `hypnogram_single`: A **single-subject** hypnogram.
#' - `hypnogram_grouped`: A **multi-subject** hypnogram with an `id` column.
#'
#' @format
#' - `hypnogram_single`: A **data frame** with 954 rows and 2 columns:
#'   \describe{
#'     \item{epoch}{Sequential sleep epoch (numeric).}
#'     \item{stage}{Sleep stage (character: "N3", "N2", "N1", "R", "W").}
#'   }
#' - `hypnogram_grouped`: A **data frame** with 1912 rows and 3 columns:
#'   \describe{
#'     \item{epoch}{Sequential sleep epoch (numeric).}
#'     \item{stage}{Sleep stage (character: "N3", "N2", "N1", "R", "W").}
#'     \item{id}{Subject ID.}
#'   }
#'
#' @usage
#' data(hypnogram_single)
#' data(hypnogram_grouped)
#'
#' @examples
#' # Load the single-subject dataset
#' data(hypnogram_single)
#' head(hypnogram_single)
#'
#' # Load the multi-subject dataset
#' data(hypnogram_grouped)
#' head(hypnogram_grouped)
#'
#' @keywords datasets
#' @aliases hypnogram_single hypnogram_grouped
"hypnogram_single"

"hypnogram_grouped"
