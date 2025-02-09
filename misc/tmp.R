library(tidyverse)
library(SleepCycles)
source("./R/sleepcycles_from_hypnogram.R")

SleepCycles_res <- read.table("/Users/jasondude/Desktop/SleepCycles_exmpl/SleepCycles_2025-02-05/sleepstages_SCycles.txt", header = T)
SleepCycles_res <- SleepCycles_res |> 
  mutate(
    stage = case_when(
      SleepStages == "0" ~ "W",
      SleepStages == "1" ~ "N1",
      SleepStages == "2" ~ "N2",
      SleepStages == "3" ~ "N3",
      SleepStages == "5" ~ "R"
    ) 
  )

# ---------------------------------------------------------------------------------------------

sleepstages <- as.data.frame(sleepstages[-c(1, 2), ])
sleepstages$V2 <- trimws(sleepstages$V2)

sleepstages <- sleepstages|> 
  rename(stage = V2) |> 
  mutate(stage = case_when(
    stage == "0" ~ "W",
    stage == "1" ~ "N1",
    stage == "2" ~ "N2",
    stage == "3" ~ "N3",
    stage == "5" ~ "R"
  ), stage = as.factor(stage)) |> 
  mutate(epoch = 1:n())

options <- list(
  "NREMP" = list(
    "density_col" = "N1_N2_N3",
    "threshold" = 0.6,
    "min_gap" = 20,
    "min_size" = 20
  ),
  "REMP" = list(
    "density_col" = "R",
    "threshold" = 0.6,
    "min_gap" = 15,
    "min_size" = 5
  )
)

sleepcycles_res <- sleepcycles_from_hypnogram(sleepstages, epoch_col = "epoch", stage_col = "stage", method = "dude", options = options, combos = list("N1_N2_N3" = c("N1", "N2", "N3")))

# ---------------------------------------------------------------------------------------------

gridExtra::grid.arrange(
  SleepCycles_res |> 
    mutate(epoch = 1:n(), cycle_type = case_when(N_REM == 0 ~ "NREMP", N_REM == 1 ~ "REMP", is.na(N_REM) ~ "NC")) |> 
    ggplot() +
    geom_point(aes(epoch, cycle_type, col = cycle_type)) +
    ylab("") +
    ggtitle("Sleep Cycles package"),
  sleepcycles_res$epoch |>
    mutate(
      epoch = 1:n(),
      cycle_type = case_when(is.na(cycle_type) ~ "NC", .default = cycle_type)
    ) |>
    ggplot() +
    geom_point(aes(epoch, cycle_type, col = cycle_type)) + 
    ylab("") +
    ggtitle("My package")
)

tmp1 <- SleepCycles_res |> 
  mutate(
    epoch = 1:n(),
    Feinberg_Floyd = case_when(N_REM == 0 ~ "NREMP", N_REM == 1 ~ "REMP", is.na(N_REM) ~ "NC")
)
tmp2 <- sleepcycles_res$epoch |>  mutate(Dude = case_when(is.na(cycle_type) ~ "NC", .default = cycle_type))
tmp <- inner_join(tmp1, tmp2) |> mutate(Match = ifelse(Feinberg_Floyd == Dude, TRUE, FALSE))

tmp |> 
  select(epoch, Match, Feinberg_Floyd, Dude) |> 
  pivot_longer(cols = -c(epoch, Match)) |> 
  ggplot(aes(epoch, interaction(name, value), col = Match)) +
  geom_point(show.legend = F) + 
  scale_color_manual(values = c("red", "black")) +
  xlab("Epoch") +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    axis.title.y = element_blank()
  )
