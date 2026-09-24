# Run this script from the repository root.

source(file.path("batactivity", "R", "utils.R"))
source(file.path("batactivity", "R", "standardize.R"))

input_file <- file.path("data", "Sortie_SonoChiro.csv")
output_file <- file.path("output", "passive_standard.csv")
software <- "SonoChiro"

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

raw_data <- read.csv(input_file, sep = ";", stringsAsFactors = FALSE)
standard_data <- standardize_table(
  raw_data,
  software = software,
  output_file = output_file
)

message("Standardized table exported to: ", output_file)
