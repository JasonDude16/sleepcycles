#' Detect Sleep Cycles Using the Feinberg Algorithm
#'
#' This function applies the Feinberg sleep cycle detection algorithm to segment sleep
#' cycles from a hypnogram dataset.
#'
#' @param df A data frame containing epoch-level sleep stage data.
#' @param epoch_col The column name in `df` that contains the epoch numbers.
#' @param stage_col The column name in `df` that contains sleep stage classifications.
#' @param options A list of parameters for customizing the Feinberg algorithm:
#'   - `sleepstart` (default = "N1"): Determines how sleep onset is defined.
#'   - `treat_as_W` (default = "A"): Reclassifies certain stages as wake ("W").
#'   - `rm_incomplete_period` (default = FALSE): Whether to remove incomplete sleep periods.
#'   - `REMP_length` (default = 10): Minimum number of epochs for a valid REM period.
#'   - `sleep_levels` (default = c("N1", "N2", "N3", "R")): Defines sleep stage categories.
#'
#' @return A list containing:
#'   - `epoch`: A data frame with cycle annotations for each epoch.
#'   - `summary`: A summary of detected sleep cycles.
#'   - `info`: The options used in the analysis.
#'
#' @keywords internal
.sleepcycles_from_hypnogram_feinberg <- function(df, epoch_col, stage_col, options = list()) {

  # Set default options if not provided
  if (!"sleepstart" %in% names(options)) options$sleepstart <- "N1"
  if (!"treat_as_W" %in% names(options)) options$treat_as_W <- "A"
  if (!"rm_incomplete_period" %in% names(options)) options$rm_incomplete_period <- FALSE
  if (!"REMP_length" %in% names(options)) options$REMP_length <- 10
  if (!"sleep_levels" %in% names(options)) options$sleep_levels <- c("N1", "N2", "N3", "R")

  # Preprocess data: Standardize wake stages and classify sleep stages
  df <- .prep_data(df, stage_col = stage_col, treat_as_W = options$treat_as_W)

  # Identify all NREM and wake epochs
  NREMWs <- which(df$Descr3 == "NREM" | df$Descr3 == "W")
  NREMs <- which(df$Descr3 == "NREM")
  first_N2 <- which(df[[stage_col]] == "N2")[1]

  # Adjust NREM identification based on sleep onset setting
  if (options$sleepstart == "N1") {
    NREMWs <- subset(NREMWs, NREMWs >= NREMs[1])
  } else if (options$sleepstart == "N2") {
    NREMWs <- subset(NREMWs, NREMWs >= first_N2)
  }

  # Identify start points of NREMPs and REMPs
  NREMWs_start2 <- .find_NREMPs(NREMWs, df)
  df$CycleStart <- NA
  df$CycleStart[NREMWs_start2] <- "NREMP"

  REMs_start2 <- .find_REMPs(options$REMP_length, df)
  df$CycleStart[REMs_start2] <- "REMP"

  # Remove duplicate cycle starts
  df$CycleStart[.delete_reps(df)] <- NA

  # Handle NREMPs that are too long (e.g., >120 min)
  toolong <- .is.toolong(df)
  if (length(toolong) > 0) {
    df <- .toolong_split(df, toolong, stage_col)
  }

  # Add cycle numbers to the dataset
  df <- .addinfo1(df)

  # Clean up incomplete periods if specified
  if (options$rm_incomplete_period) {
    df <- .rm.incompleteperiod(df)
  } else {
    df <- .clean_endofnight(df)
  }

  # Format output: Assign cycle labels
  df_epoch <- df |>
    dplyr::rename(cycle = .data$cycles) |>
    dplyr::mutate(cycle_type = dplyr::case_when(REM.NREM == 0 ~ "NREMP", REM.NREM == 1 ~ "REMP")) |>
    dplyr::select(dplyr::all_of(c(epoch_col, stage_col, "cycle", "cycle_type"))) |>
    tibble::as_tibble()

  # Generate cycle summary
  df_sc <- .cycle_summary(
    x = .get_cycle_segments(df = df_epoch, epoch_col = epoch_col),
    df = df,
    epoch_col = epoch_col,
    stage_col = stage_col,
    options = options
  )

  return(list("epoch" = df_epoch, "summary" = df_sc, "info" = options))
}


.prep_data <- function(df, stage_col, treat_as_W) {

  # Reclassify specified stage as wake
  if (!is.na(treat_as_W)) {
    df[[stage_col]][df[[stage_col]] == treat_as_W] <- "W"
  }

  # Classify stages based on their relationships
  df$Descr2 <- NA
  df$Descr2[df[[stage_col]] %in% c("N1", "N2", "W", "R")] <- "RWN12"
  df$Descr2[df[[stage_col]] == "N3"] <- "N3"

  df$Descr3 <- NA
  df$Descr3[df[[stage_col]] %in% c("N1", "N2", "N3")] <- "NREM"
  df$Descr3[df[[stage_col]] == "W"] <- "W"
  df$Descr3[df[[stage_col]] == "R"] <- "REM"

  return(df)
}



.find_NREMPs <- function (NREMWs, data) {
  NREMWs_start <- NA
  for (k in 1:(length(NREMWs) - 29)) {
    if ((all(seq(NREMWs[k], length.out = 30) == NREMWs[seq(k, k + 29)])) & (data$Descr3[NREMWs[k]] != "W")) {
      NREMWs_start <- c(NREMWs_start, NREMWs[k])
    }
    else {
      next
    }
  }
  NREMWs_start <- NREMWs_start[-c(1)]
  NREMWs_start2 <- NREMWs_start[1]
  for (k in 1:(length(NREMWs_start) - 1)) {
    if ((NREMWs_start[k + 1] - NREMWs_start[k]) > 1) {
      NREMWs_start2 <- c(NREMWs_start2, NREMWs_start[k + 1])
    }
  }
  return(NREMWs_start2)
}


.find_REMPs <- function (REMP_length, data) {
  REMs <- which(data$Descr3 == "REM")
  REMs_start <- REMs[1]
  if ((length(REMs) - (REMP_length - 1)) >= 0) {
    for (k in 1:(length(REMs) - (REMP_length - 1))) {
      if (all(seq(REMs[k], length.out = REMP_length) ==
              REMs[seq(k, k + (REMP_length - 1))])) {
        REMs_start <- c(REMs_start, REMs[k])
      }
    }
    REMs_start <- unique(REMs_start)
  }
  REMs_start2 <- REMs_start[1]
  if (length(REMs_start) > 1) {
    for (k in 1:(length(REMs_start) - 1)) {
      if ((REMs_start[k + 1] - REMs_start[k]) > 1) {
        REMs_start2 <- c(REMs_start2, REMs_start[k + 1])
      }
    }
  }
  return(REMs_start2)
}


.delete_reps <- function (data) {
  rm <- NA
  cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
  for (k in 2:length(cycs)) {
    if (data$CycleStart[cycs[k]] == data$CycleStart[cycs[k - 1]])
      rm <- c(rm, cycs[k])
  }
  rm <- unique(rm)
  rm <- rm[c(-1)]
  return(rm)
}


.is.toolong <- function (data) {

  NREM1 <- which(data$CycleStart == "NREMP")
  REM1 <- which(data$CycleStart == "REMP")

  if (REM1[1] < NREM1[1]) {
    message("ATTENTION: first REM period precedes NREM period!")
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")

    toolong <- NA
    for (k in seq(3, length(cycs), 2)) {
      subset <- data[c(cycs[k - 1]:(cycs[k] - 1)), ]
      wake_eps <- sum(subset$Descr2 == "W")
      if (all(is.na(wake_eps))) {
        wake_eps <- 0
      }
      if (((cycs[k] - cycs[k - 1]) - wake_eps) >= 240) {
        toolong <- c(toolong, cycs[k - 1])
      }
    }
    toolong <- stats::na.omit(toolong)

  } else {
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
    toolong <- NA
    for (k in seq(2, length(cycs), 2)) {
      subset <- data[c(cycs[k - 1]:(cycs[k] - 1)), ]
      wake_eps <- sum(subset$Descr2 == "W")
      if (all(is.na(wake_eps))) {
        wake_eps <- 0
      }
      if (((cycs[k] - cycs[k - 1]) - wake_eps) >= 240) {
        toolong <- c(toolong, cycs[k - 1])
      }
    }
    toolong <- stats::na.omit(toolong)
  }

  return(toolong)
}


.toolong_split <- function (data, toolong, stage_col) {
  for (zz in 1:length(toolong)) {
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
    curr_toolong <- toolong[c(zz)]
    message(paste0(
      "Attempting to split NREMP ",
      as.character(zz),
      " out of ",
      as.character(length(toolong)),
      "."
    ))

    beg_end <- c(cycs[which(cycs == curr_toolong)], cycs[which(cycs == curr_toolong) + 1])
    RWN12s <- which(data$Descr2 == "RWN12")
    RWN12s <- RWN12s[c(RWN12s >= beg_end[1] & RWN12s <= beg_end[2])]

    RWN12s_start <- NA
    for (kk in 1:(length(RWN12s) - 23)) {
      if (all(seq(RWN12s[kk], length.out = 24) == RWN12s[seq(kk, kk + 23)])) {
        RWN12s_start <- c(RWN12s_start, RWN12s[kk])
      }
    }

    RWN12s_start <- RWN12s_start[-c(1)]
    rm(kk)
    RWN12s_start2 <- RWN12s_start[c(RWN12s_start > beg_end[1])]
    if (length(RWN12s_start2) > 0) {
      RWN12s_start2 <- RWN12s_start[1]
      for (kk in 1:(length(RWN12s_start) - 1)) {
        if ((RWN12s_start[kk + 1] - RWN12s_start[kk]) >1) {
          RWN12s_start2 <- c(RWN12s_start2, RWN12s_start[kk + 1])
        }
      }
      N3s <- which(data$Descr2 == "N3")
      N3s <- N3s[c(beg_end[1] < N3s) & c(beg_end[2] > N3s)]
      N3s <- N3s[c(N3s > RWN12s_start2[1])]
      if (length(N3s) > 0) {
        if (length(N3s) == 1) {
          N3_start <- N3s
        }
        else {
          N3_start <- N3s[1]
          for (kk in 1:(length(N3s) - 1)) {
            if ((N3s[kk + 1] - N3s[kk]) > 1) {
              N3_start <- c(N3_start, N3s[kk + 1])
            }
          }
        }
        RWN12s_start2 <- RWN12s_start2[RWN12s_start2 < utils::tail(N3_start, 1)]
        n <- NA
        for (zzz in 1:length(RWN12s_start2)) {
          minpositive = function(x) min(x[x > 0])
          val <- which(minpositive(N3_start - RWN12s_start2[zzz]) == (N3_start - RWN12s_start2[zzz]))
          n <- c(n, val)
        }
        n <- stats::na.omit(n)
        if (N3_start[1] == N3s[1]) {
          n <- n[-c(1)]
        }
        N3_start2 <- N3_start[n]
        if (length(N3_start2) > 0) {
          data$CycleStart[c(N3_start2)] <- "NREMP"
          splits <- N3_start2
          splits <- unique(splits)
          dfplot <- data
          dfplot$time <- seq(1, nrow(dfplot))

          pp <- ggplot2::ggplot(
            dfplot,
            ggplot2::aes(
              x = .data$time,
              y = .data[[stage_col]],
              colour = .data[[stage_col]],
              group = 1
            )
          ) +
            ggplot2::theme_bw() +
            ggplot2::geom_point() +
            ggplot2::geom_line(ggplot2::aes(x = .data$time, y = .data[[stage_col]])) +
            ggplot2::xlab("Epoch") +
            ggplot2::ylab("Sleep Stage") +
            ggplot2::scale_color_viridis_d(name = "Sleep Stage") +
            ggplot2::geom_vline(xintercept = c(splits), lty = 2, colour = "red") +
            ggplot2::theme(legend.position = "none") +
            ggplot2::annotate(
              geom = "text",
              x = 500,
              y = 2,
              label = paste("can split at epoch(s):", paste(splits, collapse = ","), sep = " ")
            ) +
            ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
          print(pp)

          part1 <- splits - beg_end[1]
          part2 <- beg_end[2] - splits + 1

          print(
            paste0(
              "When splitting at 1st/ 2nd/... suggestion, NREMP1 would be ",
              (as.character(part1 / 2)),
              " min.",
              "and NREMP2 would be ",
              (as.character(part2 / 2)),
              " min.",
              sep = " "
            )
          )

          if (any(part1 < 30) | any(part2 < 30)) {
            message("BE CAREFUL: Splitting might result in NREM period that is shorter than 15 min.")
          }
          val <- readline("Where do you want to split? Type the 1/2/3/... to select a suggestion, n to split at a specific epoch or not at all, or skip to skip this night for now. ")
          if (val == "skip") {
            message("This period/night is skipped.")
            next
          }
          else if (val == "n") {
            newperiod <- readline("At which epoch do you want to start the new NREM period instead? Please type epoch number or NA to not split. ")
            data$CycleStart[c(N3_start)] <- NA
            if (newperiod != "NA") {
              data$CycleStart[as.numeric(newperiod)] <- "NREMP"
            }
          }
          else if (val %in% as.character(1:9)) {
            data$CycleStart[c(N3_start2)] <- NA
            data$CycleStart[N3_start2[as.numeric(val)]] <- "NREMP"
          }
          else {
            message("Missing entry. This night is skipped.")
          }
          rm(dfplot, pp)
        }
        else {
          message("Cannot split. No N3 following 'lightening' of sleep has been detected.")
        }
      }
      else {
        message("Cannot split. No N3 following 'lightening' of sleep has been detected.")
      }
    }
    else {
      message("No 'lightening' of sleep or N3 detected after the onset of the NREMP. Cannot split.")
    }
  }
  return(data)
}


.addinfo1 <- function (data) {
  NREMPs <- which(data$CycleStart == "NREMP")
  data$cycles <- NA
  for (k in 1:(length(NREMPs))) {
    if (k < length(NREMPs)) {
      data$cycles[NREMPs[k]:(NREMPs[k + 1] - 1)] <- k
    }
    else {
      data$cycles[NREMPs[k]:(nrow(data))] <- k
    }
  }
  cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
  data$REM.NREM <- NA
  for (k in 1:(length(cycs))) {
    if (k < length(NREMPs)) {
      if (data$CycleStart[cycs[k]] == "NREMP") {
        data$REM.NREM[c((cycs[k]):((cycs[k + 1]) - 1))] <- 0
      }
      else if (data$CycleStart[cycs[k]] == "REMP") {
        data$REM.NREM[c((cycs[k]):((cycs[k + 1]) - 1))] <- 1
      }
    }
    else {
      if (data$CycleStart[cycs[k]] == "NREMP") {
        data$REM.NREM[c((cycs[k]):(nrow(data)))] <- 0
      }
      else if (data$CycleStart[cycs[k]] == "REMP") {
        data$REM.NREM[c((cycs[k]):(nrow(data)))] <- 1
      }
    }
  }
  return(data)
}


.rm.incompleteperiod <- function (data) {
  cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
  if (data$CycleStart[cycs[length(cycs)]] == "REMP") {
    REMs <- which(data$Descr3 == "REM")
    stop <- REMs[length(REMs)] + 1
    end <- which(data$Descr3 == "NREM")
    end <- end[end >= stop]
    if (length(end) > 10) {
      data$CycleStart[stop] <- "stop"
      data$cycles[stop:nrow(data)] <- NA
      data$REM.NREM[stop:nrow(data)] <- NA
    }
    else {
      stop <- cycs[length(cycs)]
      data$CycleStart[cycs[length(cycs)]] <- "stop"
      data$cycles[stop:nrow(data)] <- NA
      data$REM.NREM[stop:nrow(data)] <- NA
    }
  }
  else {
    NREMs <- which(data$Descr3 == "NREM")
    stop <- NREMs[length(NREMs)] + 1
    end <- which(data$Descr3 == "REM")
    end <- end[end >= stop]
    if (length(end) > 10) {
      data$CycleStart[stop] <- "stop"
      data$cycles[stop:nrow(data)] <- NA
      data$REM.NREM[stop:nrow(data)] <- NA
    }
    else {
      stop <- cycs[length(cycs)]
      data$CycleStart[cycs[length(cycs)]] <- "stop"
      data$cycles[stop:nrow(data)] <- NA
      data$REM.NREM[stop:nrow(data)] <- NA
    }
  }
  return(data)
}


.clean_endofnight <- function (data) {
  cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
  if (data$CycleStart[cycs[length(cycs)]] == "REMP") {
    REMs <- which(data$Descr3 == "REM")
    if (utils::tail(REMs, n = 1) == nrow(data)) {
      stop <- utils::tail(REMs, n = 1)
      data$CycleStart[stop] <- "stop"
    }
    else {
      stop <- utils::tail(REMs, n = 1) + 1
      data$CycleStart[stop] <- "stop"
      data$cycles[stop:nrow(data)] <- NA
      data$REM.NREM[stop:nrow(data)] <- NA
    }
  }
  else {
    lastNREMP <- utils::tail(which(data$CycleStart == "NREMP"),
                      1)
    Ws <- which(data$Descr3 == "W")
    Ws <- Ws[Ws > lastNREMP]
    if (length(Ws) > 2) {
      Ws_start <- utils::head(Ws, 1)
      for (kk in 2:(length(Ws))) {
        if (Ws[kk - 1] != Ws[kk] - 1) {
          Ws_start <- c(Ws_start, Ws[kk])
        }
      }
      Ws_start <- utils::tail(Ws_start, 1)
    }
    else {
      Ws_start <- c()
    }
    NREMs <- which(data$Descr3 == "NREM")
    if (length(Ws_start) > 0) {
      if (Ws_start < utils::tail(NREMs, 1))
        Ws_start <- c()
    }
    if (length(Ws_start) == 0 & data$Descr3[nrow(data)] == "W")
      Ws_start <- nrow(data)
    if (length(Ws_start) > 0) {
      data$CycleStart[data$CycleStart == "stop"] <- NA
      stop <- Ws_start
      data$CycleStart[stop] <- "stop"
    }
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
    if (data$CycleStart[cycs[length(cycs)]] == "REMP") {
      REMs <- which(data$Descr3 == "REM")
      if (utils::tail(REMs, n = 1) == nrow(data)) {
        data$CycleStart[data$CycleStart == "stop"] <- NA
        stop <- utils::tail(REMs, n = 1)
        data$CycleStart[stop] <- "stop"
      }
      else {
        data$CycleStart[data$CycleStart == "stop"] <- NA
        stop <- utils::tail(REMs, n = 1) + 1
        data$CycleStart[stop] <- "stop"
        data$cycles[stop:nrow(data)] <- NA
        data$REM.NREM[stop:nrow(data)] <- NA
      }
    }
    check <- which(data$CycleStart == "stop")
    if (length(check) > 0) {
      if (stop - utils::tail(which(data$CycleStart == "NREMP"), 1) >= 30) {
        data$cycles[stop:nrow(data)] <- NA
        data$REM.NREM[stop:nrow(data)] <- NA
      }
      else {
        data$CycleStart[utils::tail(which(data$CycleStart == "NREMP"), 1)] <- "stop"
        data$CycleStart[stop] <- NA
        data$cycles[utils::tail(which(data$CycleStart == "stop"), 1):nrow(data)] <- NA
        data$REM.NREM[utils::tail(which(data$CycleStart == "stop"), 1):nrow(data)] <- NA
        cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
        if (data$CycleStart[cycs[length(cycs)]] == "REMP") {
          REMs <- which(data$Descr3 == "REM")
          stop <- REMs[length(REMs)] + 1
          data$CycleStart[stop] <- "stop"
          data$cycles[stop:nrow(data)] <- NA
          data$REM.NREM[stop:nrow(data)] <- NA
        }
      }
    }
  }
  return(data)
}


.get_cycle_segments <- function(df, epoch_col) {
  n_cycles <- length(setdiff(unique(df$cycle), NA))
  res <- vector("list", length = n_cycles*2)
  counter <- 1
  for (cyclei in 1:n_cycles) {
    for (typei in c("NREMP", "REMP")) {
      epochs <- df[!is.na(df$cycle) & df$cycle == cyclei & df$cycle_type == typei, ][[epoch_col]]
      if (length(epochs) > 0) {
        res[[counter]] <- data.frame(
          cycle = cyclei,
          cycle_type = typei,
          start_epoch = epochs[1],
          end_epoch = epochs[length(epochs)]
        )
        counter <- counter + 1
      }
    }
  }
  return(Reduce(rbind, res))
}
