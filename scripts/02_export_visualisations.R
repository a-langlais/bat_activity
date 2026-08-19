# Lancez ce script depuis la racine du depot.

input_file <- file.path("data", "passive.csv")
output_dir <- "output"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data <- read.csv(input_file, sep = ";", stringsAsFactors = FALSE)

required_columns <- c("Place", "Id")
missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns) > 0) {
  stop("Colonnes manquantes : ", paste(missing_columns, collapse = ", "))
}

contacts_by_species <- aggregate(
  list(contacts = data$Id),
  by = list(Id = data$Id),
  FUN = length
)
contacts_by_species <- contacts_by_species[order(contacts_by_species$contacts), ]

contacts_by_place <- aggregate(
  list(contacts = data$Place),
  by = list(Place = data$Place),
  FUN = length
)
contacts_by_place <- contacts_by_place[order(contacts_by_place$contacts), ]

png(file.path(output_dir, "01_contacts_par_espece.png"), width = 1400, height = 900, res = 150)
par(mar = c(5, 12, 4, 2))
barplot(
  contacts_by_species$contacts,
  names.arg = contacts_by_species$Id,
  horiz = TRUE,
  las = 1,
  col = "#4D7C6A",
  border = NA,
  xlab = "Nombre de contacts",
  main = "Contacts par espece"
)
dev.off()

png(file.path(output_dir, "02_contacts_par_site.png"), width = 1200, height = 800, res = 150)
par(mar = c(6, 5, 4, 2))
barplot(
  contacts_by_place$contacts,
  names.arg = contacts_by_place$Place,
  las = 2,
  col = "#D2A44E",
  border = NA,
  ylab = "Nombre de contacts",
  main = "Contacts par site"
)
dev.off()

message("Figures exportees dans : ", output_dir)
