library(tidyverse)
source("/Users/jasondude/github/personal/sleepcycles/helpers.R")

f <- list.files("~/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/BASE/data/hst/csv/sleep/epoch/aperiodics/", full.names = T)
df_all <- map_dfr(f, read.csv)

for (id in "BASE105") {
  print(id)
  
  df_subj <- df_all |> 
    filter(subj == id, channel == "C3") |> 
    select(stage, epoch, fooof_exponent) |> 
    mutate(
      stage = as.factor(stage),
      stage = fct_relevel(stage, c("3", "2", "1", "4", "0", "-1"))
    ) |> 
    arrange(epoch)
  
  df_subj <- df_subj |> 
    mutate(
      fooof_smooth = convolve_with_padding(fooof_exponent, gaus_kernel(10, 2)),
      fooof_scaled = scale(fooof_smooth)[,1],
      fooof_capped = cap_z_score(fooof_scaled, -3, 3),
      fooof_rescaled = scale(fooof_capped)[,1],
      fooof_cat = categorize_z_score(fooof_rescaled, -1, 0.5),
      fooof_cat = as.factor(fooof_cat)
    )
  
  df_class_dens <- get_class_density(df_subj, "epoch", "fooof_cat", 5)
  df_subj <- inner_join(df_class_dens, df_subj)
  
  nrem_segments <- cluster_segments(df_class_dens, "high", "epoch", 0.4, 20, "NREM")
  rem_segments <- get_rem_segments(df_class_dens, nrem_segments, 0.8, 20, "REM")
  all_segments <- rbind(nrem_segments, rem_segments)
  
  plot_fooof_dens_hyp(df_subj, "epoch", "fooof_rescaled", "dens", "stage", all_segments)
  
}

segment_summary(df_subj$fooof_rescaled, rem_segments)
segment_summary(df_subj$fooof_rescaled, nrem_segments)
segment_summary(df_subj$fooof_rescaled, all_segments)
