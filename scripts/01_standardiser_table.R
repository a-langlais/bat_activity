# Lancez ce script depuis la racine du depot.

source(file.path("batactivity", "R", "TableFormatage.R"))

input_file <- file.path("data", "Sortie_SonoChiro.csv")
output_file <- file.path("data", "passive_standard.csv")
software <- "SonoChiro"

raw_data <- read.csv(input_file, sep = ";", stringsAsFactors = FALSE)
standard_data <- standardize_table(
  raw_data,
  software = software,
  output_file = output_file
)

message("Table standard exportee : ", output_file)
