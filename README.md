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
Its environment is recorded in `shiny-app/renv.lock`.

```r
install.packages("renv")
renv::restore(project = "shiny-app")
shiny::runApp("shiny-app")
```

To install the application dependencies without `renv`:

```r
install.packages(c("shiny", "readr", "dplyr", "lubridate", "suncalc", "tibble"))
```

## Scripts

Scripts are meant to be run from the repository root.

```bash
Rscript scripts/01_standardiser_table.R
Rscript scripts/02_export_visualisations.R
```

They use files in `data/` and write generated results to `output/`.

To standardize several exports in a row, pass the software name followed by
the files to process. Base files ending in `_clean.csv` produce a
corresponding `_clean_format.csv` file:

```bash
Rscript scripts/03_standardiser_lot.R SonoChiro data/nuit_1.csv data/nuit_2.csv
Rscript scripts/03_standardiser_lot.R Tadarida data/nuit_1.csv data/nuit_2.csv
```

For all base files in one directory, use `--dossier`:

```bash
Rscript scripts/03_standardiser_lot.R SonoChiro --dossier data
```

Passive indicators can also be calculated for every standardized table in a
directory, using the same `BatPassive()` method as the Shiny application:

```bash
Rscript scripts/04_indicateurs_passifs_lot.R --dossier data
```

Specify the closest supported city and recording hours when needed:

```bash
Rscript scripts/04_indicateurs_passifs_lot.R --ville Bordeaux --horaires 21:30 07:00 --dossier data
```

Recording times and solar ephemerides use French time (`Europe/Paris`) by
default. Use `--fuseau <IANA_TIMEZONE>` to select another time zone.

Apply detectability coefficients to the resulting passive indicator files:

```bash
Rscript scripts/05_correction_detectabilite.R --milieu ouvert --dossier data
```

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

Run the regression tests directly during development:

```r
testthat::test_local("batactivity")
```

The solar-duration tests run when the optional `suncalc` package is installed.

## License

This project is licensed under the Creative Commons
Attribution-NonCommercial 4.0 International License (CC BY-NC 4.0).
