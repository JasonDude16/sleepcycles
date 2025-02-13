.sleepcycles_from_hypnogram_dude <- function(df, epoch_col, stage_col, options = list()) {

  .check_sleepcycles_dude(df = df, stage_col = stage_col, options = options)

  df_dens <- .class_density(
    df = df,
    epoch_col = epoch_col,
    stage_col = stage_col,
    options = options
  )

  df_seg <- .cluster_segments(
    df = df_dens,
    epoch_col = epoch_col,
    stage_col = stage_col,
    options = options
  )

  if (is.null(df_seg)) {
    return(NULL)
  }

  df_sc <- .cycle_summary(
    x = df_seg,
    df = df,
    epoch_col = epoch_col,
    stage_col = stage_col,
    options = options,
    .data$cycle_type == "NREMP" & .data$n_epochs > options$NREMP$min_size |
      .data$cycle_type == "REMP" & .data$n_epochs > options$REMP$min_size
  )

  res_list <- vector("list", nrow(df_sc))
  for (i in 1:nrow(df_sc)) {
    tmp <- data.frame(
      df_sc$cycle[i],
      df_sc$cycle_type[i],
      seq(df_sc$start_epoch[i], df_sc$end_epoch[i])
    )
    colnames(tmp) <- c("cycle", "cycle_type", epoch_col)
    res_list[[i]] <- tmp
  }

  df_epoch <- suppressMessages(tibble::as_tibble(dplyr::full_join(df_dens, Reduce(rbind, res_list))))
  df_epoch <- dplyr::arrange(df_epoch, .data[[epoch_col]])
  density_levels <- c(levels(df[[stage_col]]), Reduce(c, lapply(options$combos, function(x) paste(x, collapse = "_"))))

  res <- list(
    "epoch" = df_epoch,
    "summary" = df_sc,
    "info" = c(options, list("density_levels" = density_levels)
    )
  )

  return(res)

}

.check_sleepcycles_dude <- function(df, stage_col, options) {

  valid_option_args <- c("sleep_levels", "combos", "NREMP", "REMP", "kernel")
  valid_nremp_remp_args <- c("density_col", "threshold", "min_gap", "min_size")

  if (!all(names(options) %in% valid_option_args)) {
    stop("all names in `options` list must be the following: ", paste0(valid_option_args, collapse = ", "), call. = FALSE)
  }
  if (length(options$kernel) %% 2 != 1) {
    stop("The length of the kernel must be odd.", call. = FALSE)
  }
  if (!all(names(options$NREMP) %in% valid_nremp_remp_args)) {
    stop("Valid option names for `NREMP` are: ", paste0(valid_nremp_remp_args, collapse = ", "), call. = FALSE)
  }
  if (!all(names(options$REMP) %in% valid_nremp_remp_args)) {
    stop("Valid option names for `REMP` are: ", paste0(valid_nremp_remp_args, collapse = ", "), call. = FALSE)
  }
}

.binarize_levels <- function(df, epoch_col, stage_col) {
  res <- list()
  res[[epoch_col]] <- df[[epoch_col]]
  res[[stage_col]] <- df[[stage_col]]
  for (ss in levels(df[[stage_col]])) {
    res[[ss]] <- as.numeric(df[[stage_col]] == ss)
  }
  return(data.frame(res))
}

.gaussian_kernel <- function(size, sigma) {
  x <- seq(-floor(size / 2), floor(size / 2), length.out = size)
  kernel <- exp(-0.5 * (x / sigma)^2)
  return(kernel / sum(kernel))
}

.convolve_same_length <- function(x, kernel) {
  res <- stats::convolve(x, kernel, type = "open")
  n <- length(x)
  k <- length(kernel)
  start_idx <- ceiling(k / 2)
  end_idx <- start_idx + n - 1
  return(res[start_idx:end_idx])
}

.class_density <- function(df, epoch_col, stage_col, options) {

  res <- .binarize_levels(df = df, epoch_col = epoch_col, stage_col = stage_col)

  res[-c(1, 2)] <- apply(res[-c(1, 2)], 2, .convolve_same_length, kernel = options$kernel)
  res[-c(1, 2)] <- apply(res[-c(1, 2)], 2, function(x) ifelse(x < 1e-9, 0, x))

  for (i in seq_along(options$combos)) {
    res_combo_cols <- lapply(options$combos[[i]], function(s) {
      if (s %in% colnames(res) == FALSE) {
        warning("One of the combo levels (", s, ") is not present in hypnogram, ignoring", call. = FALSE)
        return(NULL)
      } else {
        return(res[[s]])
      }
    })
    res[[names(options$combos)[i]]] <- Reduce("+", purrr::compact(res_combo_cols))
  }

  return(res)
}

.cluster_segments <- function(df, epoch_col, stage_col, options) {

  types <- c("NREMP", "REMP")
  res <- vector("list", 2)
  names(res) <- types

  for (type in types) {

    opti <- options[[type]]

    # identify indices where the signal exceeds the threshold
    active_indices <- which(df[[opti$density_col]] > opti$threshold)

    if (length(active_indices) == 0) {
      next
    }

    # compute gaps between consecutive indices
    gaps <- diff(active_indices)

    # group segments using a cumulative sum when gap exceeds `min_gap`
    segment_labels <- cumsum(c(1, gaps > opti$min_gap))

    # count segment sizes and retain only those meeting `min_size` criteria
    segment_sizes <- table(segment_labels)
    valid_segments <- as.numeric(names(segment_sizes[segment_sizes >= opti$min_size]))

    if (length(valid_segments) == 0) {
      next
    }

    # extract only indices that belong to valid segments
    valid_indices <- active_indices[segment_labels %in% valid_segments]

    # identify where segments start and end
    segment_breaks <- which(diff(valid_indices) > opti$min_gap)
    start_indices <- valid_indices[c(1, segment_breaks + 1)]
    end_indices <- valid_indices[c(segment_breaks, length(valid_indices))]

    # ensure `end_indices` doesn't exceed data frame bounds
    end_indices <- pmin(end_indices + 1, nrow(df))

    res[[type]] <- tibble::tibble(
      cycle_type = type,
      start_epoch = df[[epoch_col]][start_indices],
      end_epoch = df[[epoch_col]][end_indices]
    )

  }

  missing_period <- Reduce(c, lapply(res, is.null))
  if (any(missing_period)) {

    if (all(missing_period)) {
      warning("No NREMP or REMP periods were found, skipping...", call. = FALSE)
      return(NULL)

    }
    if (is.null(res[["NREMP"]])) {
      warning("No NREMP periods were found", call. = FALSE)

    }
    if (is.null(res[["REMP"]])) {
      warning("No REMP periods were found", call. = FALSE)

    }
    res <-  Reduce(rbind, res)

  } else {
    res <- .handle_overlapping_cycles(x = Reduce(rbind, res))

  }

  return(res)
}

.handle_overlapping_cycles <- function(x) {

  x_nremp <- x[x$cycle_type == "NREMP", ]
  x_remp <- x[x$cycle_type == "REMP", ]

  remp_seg_all <- unlist(purrr::map(1:nrow(x_remp), ~ seq(x_remp$start_epoch[.x], x_remp$end_epoch[.x])))

  res <- list()
  for (r in 1:nrow(x)) {

    nremp_seg <- seq(x$start_epoch[r], x$end_epoch[r])
    overlap <- intersect(nremp_seg, remp_seg_all)

    if (length(overlap) == 0) {
      res <- append(res, list(
        data.frame(
          "cycle_type" = "NREMP",
          "start_epoch" = nremp_seg[1],
          "end_epoch" = nremp_seg[length(nremp_seg)]
        )
      ))
      next
    }

    warning("NREMP and REMP overlap (epochs ", overlap[1], "-", overlap[length(overlap)], "). Splitting NREMP and keeping REMP...", call. = FALSE)
    nremp_seg[nremp_seg %in% overlap] <- NA

    nremp_seg_grp <- cumsum(is.na(nremp_seg))
    groups <- rep(NA, length(nremp_seg_grp))
    for (i in seq_along(nremp_seg_grp)) {

      if (i == 1) {
        if (nremp_seg_grp[i] == 0) {
          groups[i] <- 0
        }
      } else {
        if (nremp_seg_grp[i] == nremp_seg_grp[i - 1]) {
          groups[i] <- nremp_seg_grp[i]
        }

      }

    }

    for (group in unique(groups[!is.na(groups)])) {
      nremp_split <- nremp_seg[which(groups == group)]
      res <- append(res, list(
        data.frame(
          "cycle_type" = "NREMP",
          "start_epoch" = nremp_split[1],
          "end_epoch" = nremp_split[length(nremp_split)]
        )
      ))
    }

  }

  return(rbind(Reduce(rbind, res), x_remp))
}
