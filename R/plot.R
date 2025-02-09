#' Sleep Cycle Visualization Functions
#'
#' These functions generate plots related to sleep cycles from a `SleepCycle` object.
#'
#' - `plot_hypnogram()`: Creates a hypnogram of sleep stages over time.
#' - `plot_densities()`: Plots density estimates of sleep stages.
#' - `plot_cycles()`: Visualizes the structure of sleep cycles.
#'
#' @param sleepcycle_obj An object of class `SleepCycle`.
#' @param id (Optional) A subject identifier for grouped objects.
#' @param stage_order (Optional) Reorders sleep stages in `plot_hypnogram()`.
#' @param include_levels (Optional) Specifies which density levels to plot in `plot_densities()`.
#' @param overlay_cycles Logical. If `TRUE`, overlays sleep cycles on the plots.
#' @param clrs Color palette for different sleep states.
#' @param overlay_clrs Color palette for cycle overlays.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' plot_hypnogram(sleepcycle_obj)
#' plot_densities(sleepcycle_obj)
#' plot_cycles(sleepcycle_obj)
#'
#' @rdname sleep_plots
plot_hypnogram <- function(sleepcycle_obj, id = NULL, stage_order = NULL, overlay_cycles = TRUE, overlay_clrs = c("#3B528BFF", "#5DC863FF")) {

  if (class(sleepcycle_obj) != "SleepCycle") {
    stop("The first argument (sleepcycle_obj) must be of class `SleepCycle`.")
  }

  x <- .handle_grouped_obj(sleepcycle_obj, id)
  stage_col <- x$info$stage_col
  epoch_col <- x$info$epoch_col

  if (!is.null(stage_order)) {
    x$epoch[[stage_col]] <- forcats::fct_relevel(x$epoch[[stage_col]], stage_order)
  }

  x$epoch <- x$epoch |> dplyr::mutate(rem_mask = as.factor(ifelse(.data[[stage_col]] == "R", 1, 0)))
  x_rem <- x$epoch |> dplyr::filter(rem_mask == 1)

  p <- x$epoch |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(.data[[epoch_col]], .data[[stage_col]], group = 1),
      linewidth = 0.8,
      show.legend = F
    ) +
    ggplot2::geom_point(
      data = x_rem,
      ggplot2::aes(.data[[epoch_col]], .data[[stage_col]], col = "red"),
      shape = 15,
      show.legend = F
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Stage") +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 16))

  if (overlay_cycles) {

    x_summary <- dplyr::filter(x$summary, cycle_type %in% c("NREMP", "REMP"))
    if (length(unique(x_summary$cycle_type)) == 1) {

      if (unique(x_summary$cycle_type) == "NREMP") {
        overlay_clrs <- "#3B528BFF"
      }
      if (unique(x_summary$cycle_type) == "REMP") {
        overlay_clrs <- "#5DC863FF"
      }
    }

    stage_levels <- levels(x$epoch[[stage_col]])

    p <- p + ggplot2::geom_rect(
      data = x_summary,
      mapping = ggplot2::aes(
        xmin = start_epoch,
        xmax = end_epoch,
        ymin = stage_levels[1],
        ymax = stage_levels[length(stage_levels)],
        fill = cycle_type
      ),
      alpha = .2,
      show.legend = F
    ) +
      ggplot2::scale_fill_manual(values = overlay_clrs)
  }

  return(p)

}


#' @export
#' @rdname sleep_plots
plot_densities <- function(sleepcycle_obj, id = NULL, include_levels = NULL, clrs = c("#3B528BFF", "#5DC863FF"), overlay_cycles = TRUE, overlay_clrs = clrs) {

  if (class(sleepcycle_obj) != "SleepCycle") {
    stop("The first argument (sleepcycle_obj) must be of class `SleepCycle`.")
  }

  if (sleepcycle_obj$info$method != "dude") {
    stop("The `dude` algorithm must be used to plot densities!")
  }

  x <- .handle_grouped_obj(sleepcycle_obj, id)
  opts <- x$info$method_opts

  if (is.null(include_levels)) {
    include_levels <- c(opts$NREMP$density_col, opts$REMP$density_col)
  }

  x_long <- x$epoch |>
    tidyr::pivot_longer(cols = opts$density_levels) |>
    dplyr::filter(name %in% include_levels)

  x_long$threshold <- NA
  for (type in c("NREMP", "REMP")) {
    x_long$threshold[x_long$name == opts[[type]]$density_col] <- opts[[type]]$threshold
  }

  p <- x_long |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(.data[[x$info$epoch_col]], value, col = name),
      show.legend = F,
      linewidth = 1.2
    ) +
    ggplot2::facet_wrap(~name, ncol = 1) +
    ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1)) +
    ggplot2::scale_color_manual(values = clrs) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = threshold), linetype = 2) +
    ggplot2::xlab(x$info$epoch_col) +
    ggplot2::xlab("") +
    ggplot2::ylab("Density") +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 16))

  if (overlay_cycles) {

    if (length(unique(x$summary$cycle_type)) == 1) {

      if (unique(x$summary$cycle_type) == "NREMP") {
        overlay_clrs <- "#3B528BFF"
      }
      if (unique(x$summary$cycle_type) == "REMP") {
        overlay_clrs <- "#5DC863FF"
      }
    }

    p <- p + ggplot2::geom_rect(
      data = x$summary,
      mapping = ggplot2::aes(
        ymin = 0,
        ymax = 1,
        xmin = start_epoch,
        xmax = end_epoch,
        fill = cycle_type
      ),
      alpha = .2,
      show.legend = F
    ) +
      ggplot2::scale_fill_manual(values = overlay_clrs)
  }

  return(p)
}


#' @export
#' @rdname sleep_plots
plot_cycles <- function(sleepcycle_obj, id = NULL) {

  if (class(sleepcycle_obj) != "SleepCycle") {
    stop("The first argument (sleepcycle_obj) must be of class `SleepCycle`.")
  }

  x <- .handle_grouped_obj(sleepcycle_obj, id)

  x_nc <- x$epoch |>
    dplyr::filter(is.na(cycle_type)) |>
    dplyr::mutate(cycle_type = "NC")

  p <- x$epoch |>
    dplyr::filter(!is.na(cycle_type)) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      .data[[x$info$epoch_col]],
      cycle_type,
      col = .data[[x$info$stage_col]],
      group = interaction(cycle_type, cycle),
    ),
    linewidth = 5
    ) +
    ggplot2::geom_point(
      data = x_nc,
      mapping = ggplot2::aes(.data[[x$info$epoch_col]], cycle_type),
      size = 2
    ) +
    ggplot2::scale_y_discrete(labels = c("NC", unique(x$summary$cycle_type))) +
    ggplot2::scale_color_viridis_d(name = "Stage") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      legend.position = "top"
    )

  return(p)
}


#' Summary of Sleep Cycle Plots
#'
#' This function generates a combined visualization of a hypnogram, sleep cycles, and, if using the 'dude' algorithm, cycle densities.
#'
#' @param sleepcycle_obj An object of class `SleepCycle`.
#' @param id (Optional) A subject identifier for grouped objects.
#'
#' @return A grid of `ggplot` objects.
#' @export
#'
#' @examples
#' plot_summary(sleepcycle_obj)
plot_summary <- function(sleepcycle_obj, id = NULL) {

  if (class(sleepcycle_obj) != "SleepCycle") {
    stop("The first argument (sleepcycle_obj) must be of class `SleepCycle`.")
  }

  plots <- list(
    plot_cycles(sleepcycle_obj, id = id),
    plot_hypnogram(sleepcycle_obj, id = id) + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 18))
  )

  if (sleepcycle_obj$info$method == "dude") {
    plots <- c(plots, list(
      plot_densities(sleepcycle_obj, id = id) + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 17))
    ))
  }

  gridExtra::grid.arrange(grobs = plots)
}


#' Grouped Sleep Cycle Visualization
#'
#' These functions visualize sleep cycle statistics across multiple subjects.
#'
#' - `plot_ids()`: Displays sleep cycle progression across multiple IDs.
#' - `plot_cycle_proportions()`: Shows the proportion of different sleep cycles.
#' - `plot_cycle_counts()`: Plots a bar chart of cycle counts.
#'
#' @param sleepcycle_obj An object of class `SleepCycle` that contains multiple IDs.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' plot_ids(sleepcycle_obj)
#' plot_cycle_proportions(sleepcycle_obj)
#' plot_cycle_counts(sleepcycle_obj)
#'
#' @rdname sleep_group_plots
plot_ids <- function(sleepcycle_obj) {

  if (class(sleepcycle_obj) != "SleepCycle") {
    stop("The first argument (sleepcycle_obj) must be of class `SleepCycle`.")
  }

  if (!sleepcycle_obj$info$is_grouped) {
    stop("This plot is only for objects with multiple ids")
  }

  id_col <- sleepcycle_obj$info$id_col
  p <- sleepcycle_obj$summary |>
    dplyr::filter(cycle_type %in% c("NREMP", "REMP")) |>
    dplyr::select(dplyr::all_of(c(id_col, "cycle", "cycle_type", "start_prop", "end_prop"))) |>
    tidyr::pivot_longer(-c(.data[[id_col]], cycle_type, cycle)) |>
    ggplot2::ggplot(ggplot2::aes(value, .data[[id_col]], col = cycle_type, group = interaction(.data[[id_col]], cycle))) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::xlab("Proportion into night") +
    ggplot2::scale_color_manual(values = c("#3B528BFF", "#5DC863FF"), name = "Type") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.y =  ggplot2::element_blank(),
      text =  ggplot2::element_text(size = 16)
    )

  return(p)

}


#' @export
#' @rdname sleep_group_plots
plot_cycle_proportions <- function(sleepcycle_obj) {

  if (class(sleepcycle_obj) != "SleepCycle") {
    stop("The first argument (sleepcycle_obj) must be of class `SleepCycle`.")
  }

  if (!sleepcycle_obj$info$is_grouped) {
    stop("This plot is only for objects with multiple ids")
  }

  p <- sleepcycle_obj$summary |>
    dplyr::filter(cycle_type %in% c("NREMP", "REMP")) |>
    dplyr::group_by(cycle_type, cycle) |>
    dplyr::summarise(cycle_count = n()) |>
    dplyr::mutate(prop = cycle_count / max(cycle_count)) |>
    ggplot2::ggplot(ggplot2::aes(cycle, prop, col = cycle_type)) +
    ggplot2::geom_line(stat = "identity", linewidth = 1.5) +
    ggplot2::ylab("Proportion") +
    ggplot2::xlab("Cycle") +
    ggplot2::scale_color_manual(values = c("#3B528BFF", "#5DC863FF"), name = "Type") +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 16))

  return(p)

}

#' @export
#' @rdname sleep_group_plots
plot_cycle_counts <- function(sleepcycle_obj) {

  if (class(sleepcycle_obj) != "SleepCycle") {
    stop("The first argument (sleepcycle_obj) must be of class `SleepCycle`.")
  }

  if (!sleepcycle_obj$info$is_grouped) {
    stop("This plot is only for objects with multiple ids")
  }

  p <- sleepcycle_obj$summary |>
    dplyr::filter(cycle_type %in% c("NREMP", "REMP")) |>
    dplyr::group_by(cycle_type, cycle) |>
    dplyr::summarise(cycle_count = n()) |>
    ggplot2::ggplot(ggplot2::aes(cycle, cycle_count, fill = cycle_type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", linewidth = 1.2, alpha = .9) +
    ggplot2::ylab("Count") +
    ggplot2::xlab("Cycle") +
    ggplot2::scale_fill_manual(values = c("#3B528BFF", "#5DC863FF"), name = "Type") +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 16))

  return(p)

}
