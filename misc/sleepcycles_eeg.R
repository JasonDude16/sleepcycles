# Sleep cycle steps
# create kernel
# convolve fooof 
# scale 
# handle outliers, e.g. ceiling/floor
# categorize fooof by z-score threshold
# get class densities 
# cluster segments (NREM)
# validate NREM segments, e.g. distance between segments 
# cluster segments (REM)
# validate REM segments 
# estimate sleep cycles 

# plot fooof
# plot stage densities
# plot rem/nrem segments (time series) (all or only validated)
# plot rem/nrem segments (geom rect) (all or only validated)
# plot hypno 
# plot sleep cycles

gaus_kernel <- function(n, sigma) {
  size <- floor(n / 2) * 2 + 1
  x <- seq(floor(-size / 2), floor(size / 2) + 1)
  kernel <- exp(-x**2 / (2 * sigma**2))
  kernel <- kernel / sum(kernel) 
  return(kernel)
}


convolve_with_padding <- function(x, weights) {
  if (length(weights) %% 2 != 1) {
    stop("Weights vector must be odd")
  }
  pad <- rep(NA, (length(weights) - 1)/2)
  res <- c(pad, convolve(x, weights, type = "filter"), pad)
  return(res)
}


cap_z_score <- function(x, low, high) {
  case_when(
    x > high ~ 3,
    x < low ~ -3,
    TRUE ~ x
  )
}


categorize_z_score <- function(x, low, high) {
  case_when(
    x > high ~ "high",
    between(x, low, high) ~ "medium",
    x < low ~ "low"
  )
}


get_class_density <- function(df, epoch, stage, win) {
  
  if (is.null(levels(df[[stage]]))) {
    stop("The stage vector must have levels")
  }
  
  stage <- df[[stage]]
  stage_dens <- vector(mode = "list", length = length(levels(stage)) + 1)
  names(stage_dens) <- c(epoch, levels(stage))
  stage_dens[[epoch]] <- df[[epoch]]
  
  for (i in seq_along(stage)) {
    
    if (i - win < 1) {
      win_lower <- 1
    }
    else {
      win_lower <- i - win
    }
    
    if (i + win > length(stage)) {
      win_upper <- length(stage)
      
    } else {
      win_upper <- i + win
    }
    
    stage_win <- stage[win_lower:win_upper]
    
    for (ss in levels(stage)) {
      stage_dens[[ss]][i] <- sum(stage_win == ss) / length(stage_win)
    }
    
  }
  
  return(data.frame(stage_dens))
  
}


cluster_segments <- function(df, x, epoch, prop_thresh, min_n, type) {
  indices <- which(df[[x]] > prop_thresh)
  
  if (length(indices) == 0) {
    return(NULL)
  }
  indice_diffs <- diff(indices)
  
  grp <- 1
  segments <- NA
  for (i in seq_along(indice_diffs)) {
    
    if (indice_diffs[i] == 1) {
      segments[i] <- grp
      
    } else {
      grp <- grp + 1
      
    }
  }
  
  segment_durs <- table(segments)
  long_inds <- which(segment_durs >= min_n)
  
  if (length(long_inds) == 0) {
    return(NULL)
  }
  
  segment_long_inds <- indices[which(segments %in% long_inds)]
  
  sub <- which(diff(segment_long_inds) > 1)
  starts <- segment_long_inds[c(1, sub + 1)]
  ends <- segment_long_inds[c(sub, length(segment_long_inds))] + 1
  
  return(
    data.frame(
      "start_index" = starts,
      "start_epoch" = df[[epoch]][starts],
      "end_index" = ends,
      "end_epoch" = df[[epoch]][ends],
      "N" = (ends - starts) + 1,
      "type" = type
    )
  )
}


get_rem_segments <- function(df_class_dens, nrem_segments, prop_thresh, min_n, type) {

  res <- vector(mode = "list", length = nrow(nrem_segments))
  for (r in 1:nrow(nrem_segments)) {
    if (r < nrow(nrem_segments)) {
      potential_rem <- (nrem_segments$end_index[r] + 1):(nrem_segments$start_index[r+1] - 1)
      
    } else {
      potential_rem <- (nrem_segments$end_index[r] + 1):nrow(df_class_dens)
    }
    tmp <- df_class_dens
    tmp$medium[-potential_rem] <- NA
    res[[r]] <- cluster_segments(tmp, "medium", "epoch", prop_thresh, min_n, type)
  }
  return(reduce(res, rbind))
}


count_peaks <- function(x, normalize = TRUE) {
  nlocalmax <- nrow(pracma::findpeaks(x))
  nlocalmin <- nrow(pracma::findpeaks(-x))
  res <- nlocalmax + nlocalmin
  if (normalize) {
    res <- res / length(x)
  }
  return(res)
}


segment_summary <- function(x, segments) {
  res <- vector(mode = "list", length = length(segments))
  for (i in 1:nrow(segments)) {
    seg <- x[segments$start_index[i]:segments$end_index[i]]
    seg_cntr <- seg - mean(seg)
    peak_prop <- count_peaks(seg_cntr, normalize = TRUE)
    df <- data.frame("y" = seg_cntr, "x" = 1:length(seg_cntr))
    resid_se <- summary(lm(df$y ~ 1))$sigma
    slope <- coef(lm(df$y ~ df$x))[2]
    
    res[[i]] <- data.frame(
      "mean" = mean(seg),
      "median" = median(seg),
      "sd" = sd(seg),
      "peak_prop" = peak_prop,
      "resid_se" = resid_se,
      "slope" = slope
    )
  }
  
  return(cbind(segments, reduce(res, rbind)))
}


# ---------------------------------------------------------------------------------------------


plot_fooof <- function(df, epoch, fooof, stage = NULL, segments = NULL, ref_lines = c(0.5, -1)) {
  
  if (!is.null(stage)) {
    col <- df[[stage]]
  } else {
    col <- NULL
  }
  
  p <- df |> 
    ggplot() +
    geom_line(aes(.data[[epoch]], .data[[fooof]], group = 1, col = col), size = 1) + 
    geom_hline(yintercept = ref_lines, lty = 2)
  
  if (!is.null(segments)) {
    p <- p + 
      geom_rect(
        data = segments,
        mapping = aes(xmin = start_epoch, xmax = end_epoch, ymin = -2, ymax = 2, fill = as.factor(type)),
        alpha = .2,
        show.legend = F
      )
  }
  
  return(p)
  
}


plot_stage_densities <- function(df, epoch, dens, stage = NULL, segments = NULL) {
  
  df <- df |>
    pivot_longer(c(high, medium, low), names_to = "cat", values_to = "dens") |> 
    mutate(cat = fct_relevel(cat, c("high", "medium", "low")))
  
  if (!is.null(stage)) {
    col <- df[[stage]]
  } else {
    col <- NULL
  }
  
  p <- df |>
    ggplot() +
    geom_line(aes(.data[[epoch]], .data[[dens]], group = 1, col = col)) + 
    facet_wrap(~cat, ncol = 1) 
    
    if (!is.null(segments)) {
      p <- p + geom_rect(
        data = segments,
        mapping = aes(xmin = start_epoch, xmax = end_epoch, ymin = 0, ymax = 1, fill = as.factor(type)),
        alpha = .2,
        show.legend = F
      ) 
    }
  
  return(p)
  
}


plot_fooof_dens_hyp <- function(df, epoch, fooof, dens, stage = NULL, segments = NULL) {
  
  if (!is.null(stage)) {
    col <- df[[stage]]
  } else {
    col <- NULL
  }
  
  gridExtra::grid.arrange(
    df |> 
      plot_fooof(epoch, fooof, stage, segments) +
      theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()),
    df |>
      plot_stage_densities(epoch, dens, stage, segments) +
      theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()),
    df |> 
      ggplot(aes(epoch, stage, group = 1, col = col)) +
      geom_line() + 
      theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
  )
}
