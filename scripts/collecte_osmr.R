library(osmdata)
library(dplyr)
library(sf)

coords <- getbb("Abidjan, Ivory Coast")

# On demande les niveaux 7 et 8 pour avoir Anyama, Songon et Bingerville
communes_osm <- coords |> 
  opq() |> 
  add_osm_feature(key = "admin_level", value = c("7", "8")) |> 
  osmdata_sf()

noms_cibles <- c(
  "Abobo", "Adjamé", "Anyama", "Attécoubé", "Bingerville", 
  "Cocody", "Koumassi", "Marcory", "Plateau", "Le Plateau", 
  "Port-Bouët", "Songon", "Treichville", "Yopougon"
)

# On filtre et on harmonise le nom du Plateau immédiatement
communes_abidjan_13 <- communes_osm$osm_multipolygons |>
  filter(name %in% noms_cibles) |>
  mutate(name = ifelse(name == "Le Plateau", "Plateau", name)) |>
  select(name, geometry)

dir.create("data/raw/osm", recursive = TRUE, showWarnings = FALSE)
saveRDS(communes_abidjan_13, "data/raw/osm/communes_abidjan_13.rds")

# Vérification : doit afficher 13
print(nrow(communes_abidjan_13))