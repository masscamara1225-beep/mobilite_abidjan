library(osmdata)
library(dplyr)
library(sf)

# Serveur alternatif moins chargé
set_overpass_url("https://overpass.kumi.systems/api/interpreter")

coords <- getbb("Abidjan, Ivory Coast")

# Étape 1 — Réseau routier (seulement si pas déjà sauvegardé)
if (!file.exists("./data/raw/osm/osm_abidjan.rds")) {
  osm_raw <- coords |>
    opq() |>
    add_osm_feature(key = "highway",
                    value = c("motorway", "primary", "secondary", "tertiary")) |>
    osmdata_sf()
  saveRDS(osm_raw, "./data/raw/osm/osm_abidjan.rds")
  cat("Reseau routier sauvegarde\n")
  Sys.sleep(60)
} else {
  cat("Reseau routier deja present\n")
}

# Étape 2 — Communes level 8 (deja dans communes_abidjan_13.rds)
communes_8 <- readRDS("./data/raw/osm/communes_abidjan_13.rds")
cat(sprintf("Level 8 charge : %d communes\n", nrow(communes_8)))

Sys.sleep(30)

# Étape 3 — Communes level 7 (Anyama, Bingerville, Grand-Bassam)
communes_7 <- coords |>
  opq() |>
  add_osm_feature(key = "admin_level", value = "7") |>
  osmdata_sf()
cat("Level 7 recupere\n")

# Étape 4 — Combiner et filtrer
noms_cibles <- c(
  "Abobo", "Adjamé", "Anyama", "Attécoubé", "Bingerville",
  "Cocody", "Koumassi", "Marcory", "Plateau", "Le Plateau",
  "Port-Bouët", "Treichville", "Yopougon", "Grand-Bassam"
)

communes_abidjan <- bind_rows(
  communes_8 |> select(name, geometry),
  communes_7$osm_multipolygons |> select(name, geometry)
) |>
  filter(name %in% noms_cibles) |>
  mutate(name = ifelse(name == "Le Plateau", "Plateau", name)) |>
  distinct(name, .keep_all = TRUE)

cat(sprintf("Communes recuperees : %d\n", nrow(communes_abidjan)))
print(communes_abidjan$name)

saveRDS(communes_abidjan, "./data/raw/osm/communes_abidjan_13.rds")
plot(st_geometry(communes_abidjan), main = "Communes Grand Abidjan")