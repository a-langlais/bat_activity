# BatActivity

Tools for standardizing and analyzing bat acoustic activity data.

This repository contains three separate parts:

- `batactivity/`: an installable R package with reusable analysis functions.
- `shiny-app/`: a standalone Shiny application for interactive analysis.
- `scripts/`: reproducible one-shot workflows built around the standard table format.

The package focuses on active and passive bat survey data, with helpers to
standardize SonoChiro or Tadarida exports, summarize activity metrics, explore
weather thresholds, and handle a few field-work utilities.

## Repository Layout

```text
bat_activity/
├── batactivity/      # R package
├── shiny-app/        # Standalone Shiny app
├── scripts/          # One-shot reproducible workflows
├── data/             # Example or project data
├── images/           # Documentation images
├── output/           # Generated outputs, ignored by Git
├── README.md
└── LICENSE
```

## Install the Package

From the repository root:

```bash
R CMD INSTALL batactivity
```

Optional plotting functions use `ggplot2`:

```r
install.packages("ggplot2")
```

## Package Usage

```r
library(batactivity)
```

Standardize an acoustic software export:

```r
raw_data <- read.csv("data/Sortie_SonoChiro.csv", sep = ";")
standard_data <- standardize_table(raw_data, software = "SonoChiro")
```

Analyze active survey points:

```r
active_data <- read.csv("data/active.csv", sep = ";")
active_metrics <- bat_active(active_data, duration = 10, npoint = 6)
```

Analyze passive survey activity by place and species:

```r
passive_data <- read.csv("data/passive.csv", sep = ";")
passive_metrics <- species_place_activity(
  passive_data,
  nights = 1,
  record_time = c("22:00", "06:00")
)
```

Estimate weather thresholds:

```r
thresholds <- calculate_threshold(
  data = passive_data,
  weather = meteo,
  variables = c("Speed", "Temperature"),
  dates = c("01-06-2018", "31-07-2018"),
  percent = 95
)
```

Backward-compatible wrappers such as `TableFormatage()`, `BatActive()`,
`SpeciesPlaceActivity()`, and `CalculateThreshold()` are still exported, but the
snake_case function names are the preferred API.

## Shiny App

The Shiny app is independent from the package and lives in `shiny-app/`.

```r
install.packages(c("shiny", "readr", "dplyr", "plotly", "lubridate", "suncalc", "tibble"))
shiny::runApp("shiny-app")
```

## Scripts

Scripts are meant to be run from the repository root.

```bash
Rscript scripts/01_standardiser_table.R
Rscript scripts/02_export_visualisations.R
```

They use files in `data/` and write generated results to `output/`.

## Development

Regenerate package documentation after editing roxygen comments:

```r
roxygen2::roxygenise("batactivity")
```

Run package checks:

```bash
R CMD build batactivity
R CMD check --no-manual batactivity_0.0.0.9000.tar.gz
```

`--no-manual` is useful on systems without a LaTeX installation.

## License

This project is licensed under the Creative Commons
Attribution-NonCommercial 4.0 International License (CC BY-NC 4.0).
