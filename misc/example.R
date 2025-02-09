library(tidyverse)
source("./R/sleepcycles_from_hypnogram.R")

df_bp <- readRDS("~/github/washu/BASE-swv/data/df_bp.RDS")
df_csf <- read.csv("/Users/jasondude/github/washu/BASE-manuscript-actigraphy/data/csf.csv")

df_sleepcycles_group <- sleepcycles_from_hypnogram(
  df = df_bp,
  id_col = "subj",
  epoch_col = "epoch",
  stage_col = "stage",
  force = TRUE
)

# ---------------------------------------------------------------------------------------------

f <- list.files("/Users/jasondude/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/BASE/data/hst/csv/sleep/epoch/bandpower", full.names = T)
df_bp2 <- purrr::map_dfr(f, read.csv)

df_bp2 <- df_bp2 |> 
  mutate(
    stage = case_when(
      stage %in% c(-1, -3) ~ "A",
      stage == 0 ~ "W",
      stage == 1 ~ "N1",
      stage == 2 ~ "N2",
      stage == 3 ~ "N3",
      stage == 4 ~ "R"
    ),
    stage = as.factor(stage)
  )

df_all <- inner_join(df_bp2, df_sleepcycles_group$all)
  
walk(colnames(select(df_all, starts_with("bp"))), function(x) {
  p <- df_all |> 
    filter(cycle <= 5, stage %in% c("N2", "N3", "R")) |> 
    group_by(subj, stage, cycle, cycle_type) |> 
    summarise(power = mean(.data[[x]])) |> 
    ggplot(aes(as.factor(cycle), log(power), fill = stage)) +
    geom_boxplot() +
    ylab(x)
  plot(p)
}) 

# ---------------------------------------------------------------------------------------------

f <- list.files("/Users/jasondude/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/BASE/data/hst/csv/sleep/epoch/entropy/", full.names = T)
df_ent <- purrr::map_dfr(f, read.csv)

df_ent <- df_ent |> 
  mutate(
    stage = case_when(
      stage %in% c(-1, -3) ~ "A",
      stage == 0 ~ "W",
      stage == 1 ~ "N1",
      stage == 2 ~ "N2",
      stage == 3 ~ "N3",
      stage == 4 ~ "R"
    ),
    stage = as.factor(stage)
  )

df_all <- inner_join(df_ent, df_sleepcycles_group$all)

walk(colnames(select(df_all, starts_with("ent"))), function(x) {
  p <- df_all |> 
    filter(cycle <= 5, stage %in% c("N2", "N3", "R")) |> 
    group_by(subj, stage, cycle, cycle_type) |> 
    summarise(mean = mean(.data[[x]])) |> 
    ggplot(aes(as.factor(cycle), mean, fill = stage)) +
    geom_boxplot() +
    ylab(x)
  plot(p)
}) 

# ---------------------------------------------------------------------------------------------

df_cycles <- df_sleepcycles_group$summary

df_cycles <- df_csf |> 
  mutate(
    subj = str_remove(subj, "_"),
    ad_status_ab42_ab40 = as.factor(ad_status_ab42_ab40),
    ad_status_ttau_ab42 = as.factor(ad_status_ttau_ab42)
  ) |> 
  inner_join(df_cycles)

# cycle counts
table(df_cycles$cycle_type, df_cycles$cycle)

# cycle counts
df_cycles |> 
  group_by(subj, cycle_type, ad_status_ab42_ab40) |> 
  summarise(count = n()) |> 
  ggplot(aes(cycle_type, count, fill = ad_status_ab42_ab40)) + 
  geom_boxplot() + 
  geom_point(position = position_jitterdodge(jitter.width = .1, jitter.height = .1))

# cycle duration
df_cycles |> 
  filter(cycle <= 6) |> 
  ggplot(aes(as.factor(cycle), log(n_epochs), col = ad_status_ab42_ab40)) + 
  geom_point() +
  stat_summary(aes(group = ad_status_ab42_ab40), geom = "line", fun = mean, size = 1.2) +
  facet_wrap(~cycle_type)

# cycle start point (percent into sleep)
df_cycles |> 
  filter(cycle <= 6) |> 
  ggplot(aes(as.factor(cycle), start_prop, col = ad_status_ab42_ab40)) +
  geom_point(alpha = .1) + 
  stat_summary(aes(group = ad_status_ab42_ab40), geom = "line", fun = mean, size = 1.2) +
  facet_wrap(~cycle_type)

# first rem segment starts earlier, and is possibly shorter
df_cycles |> 
  filter(cycle == 1, cycle_type == "REMP") |> 
  ggplot(aes(ad_status_ab42_ab40, log(start_prop), fill = ad_status_ab42_ab40)) +
  geom_boxplot(show.legend = F) +
  geom_jitter(width = .05, pch = 21, show.legend = F) +
  ggpubr::stat_compare_means(method = "t.test")

df_cycles |> 
  filter(cycle == 1, cycle_type == "REMP") |> 
  ggplot(aes(ad_status_ab42_ab40, log(n_epochs/2), fill = ad_status_ab42_ab40)) +
  geom_boxplot(show.legend = F) +
  geom_jitter(width = .05, pch = 21, show.legend = F) + 
  ggpubr::stat_compare_means(method = "t.test")
