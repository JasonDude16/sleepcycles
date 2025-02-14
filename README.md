
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sleepcycles: Detect and Visualize Sleep Cycles from Hypnograms

<!-- badges: start -->

[![R-CMD-check](https://github.com/JasonDude16/sleepcycles/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JasonDude16/sleepcycles/actions/workflows/R-CMD-check.yaml)
![GitHub
release](https://img.shields.io/github/v/release/JasonDude16/sleepcycles)
[![Codecov test
coverage](https://codecov.io/gh/JasonDude16/sleepcycles/graph/badge.svg)](https://app.codecov.io/gh/JasonDude16/sleepcycles)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

The `sleepcycles` package provides tools for detecting, analyzing, and
visualizing sleep cycles from hypnogram data using density-based and
rule-based (Feinberg) algorithms. It supports both single- and
multi-subject hypnograms, enabling users to extract and explore Non-REM
(NREMP) and REM (REMP) periods with flexible parameter settings. The
package includes an interactive Shiny application, a suite of
visualization functions and utilities validating sleep staging data.

📌 Designed for sleep researchers, clinicians, and data scientists,
`sleepcycles` helps uncover the underlying dynamics of sleep
architecture and assess the impact of sleep patterns in healthy and
clinical populations.

------------------------------------------------------------------------

## **📦 Installation**

You can install the development version from GitHub using `{remotes}`:

``` r
# Install remotes if not installed
install.packages("remotes")

# Install sleepcycles from GitHub
remotes::install_github("jasondude16/sleepcycles")
```

## 🚀 Quick Start Guide

#### 1️⃣ Load the Package

``` r
library(sleepcycles)
```

#### 2️⃣ Load Example Data

`sleepcycles` contains two example datasets: `hypnogram_single` and
`hypnogram_grouped`, which contain single- and multi-subject hypnograms,
respectively. We will load the multi-subject hypnogram for this example.

``` r
data("hypnogram_grouped", package = "sleepcycles")
```

`sleepcycles` requires a hypnogram (in the format of a data frame) with
two variables: one of the epoch, and one of the sleep stage; if there
are multiple subjects, an ID variable is also required.

``` r
head(hypnogram_grouped)
#>   epoch stage id
#> 1     1     W  1
#> 2     2     W  1
#> 3     3     W  1
#> 4     4     W  1
#> 5     5     W  1
#> 6     6     W  1
```

#### 3️⃣ Detect Sleep Cycles

``` r
sleepcycle_obj <- sleepcycles_from_hypnogram(
  hypnogram_grouped, 
  epoch_col = "epoch", 
  stage_col = "stage", 
  id_col = "id",
  verbose = FALSE
)
```

#### 4️⃣ Visualize Sleep Cycles

``` r
plot_cycles(sleepcycle_obj, id = "1")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
plot_hypnogram(sleepcycle_obj, id = "1")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

#### 5️⃣ Run the Interactive Shiny App

``` r
if (interactive()) {
  run_sleepcycles_app(sleepcycle_obj)
}
```

### 📊 Features

- ✔ **Sleep Cycle Detection**: Supports Feinberg (Rule-Based) algorithm
  and a novel density-based algorithm
- ✔ **Interactive Visualization**: Generate hypnograms, density plots,
  and cycle summaries
- ✔ **Shiny App**: Modify and explore cycle detection parameters
  interactively
- ✔ **Preprocessing & Validation**: Built-in data validation tools
- ✔ **Single and Grouped Data Support**: Works with individual and
  multi-subject hypnogram datasets

### 📖 Available Functions

| **Function**                   | **Description**                                                 |
|--------------------------------|-----------------------------------------------------------------|
| `sleepcycles_from_hypnogram()` | Detects sleep cycles using DUDE or Feinberg method              |
| `plot_hypnogram()`             | Generates a hypnogram plot                                      |
| `plot_densities()`             | Plots hypnogram stage densities                                 |
| `plot_cycles()`                | Visualizes detected sleep cycles                                |
| `plot_summary()`               | Provides a combined visualization of sleep cycles and hypnogram |
| `run_sleepcycles_app()`        | An interactive Shiny app for sleep cycle parameter exploration  |
| `check_hypnogram()`            | Validates hypnogram data for errors                             |

### 📌 Contributing

We welcome contributions! If you find a bug, please [open an
issue](https://github.com/JasonDude16/sleepcycles/issues).

### 📜 License

This package is licensed under the MIT License.
